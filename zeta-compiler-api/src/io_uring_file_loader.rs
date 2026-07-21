use io_uring::{IoUring, opcode, types};
use std::fs::File;
use std::io;
use std::os::fd::AsRawFd;
use std::path::PathBuf;

use crate::file_loader::{FileLoader, SourceFile};

pub struct IoUringFileLoader {
    queue_depth: usize,
}

impl IoUringFileLoader {
    pub fn new(queue_depth: usize) -> Self {
        Self { queue_depth }
    }
}

impl FileLoader for IoUringFileLoader {
    fn load_files(&self, paths: &[PathBuf]) -> Result<Vec<SourceFile>, io::Error> {
        let mut ring = IoUring::new(self.queue_depth as u32)?;

        let mut files = Vec::with_capacity(paths.len());
        let mut buffers = Vec::with_capacity(paths.len());

        for path in paths {
            let file = File::open(path)?;

            let metadata = file.metadata()?;
            let size = metadata.len() as usize;

            files.push(file);
            buffers.push(vec![0u8; size]);
        }

        let mut submitted = 0;

        for (idx, file) in files.iter().enumerate() {
            let entry = opcode::Read::new(
                types::Fd(file.as_raw_fd()),
                buffers[idx].as_mut_ptr(),
                buffers[idx].len() as u32,
            )
            .build()
            .user_data(idx as u64);

            unsafe {
                ring.submission()
                    .push(&entry)
                    .map_err(|_| io::Error::other("submission queue full"))?;
            }

            submitted += 1;
        }

        ring.submit_and_wait(submitted)?;

        let mut results = vec![None; paths.len()];

        for cqe in ring.completion() {
            let idx = cqe.user_data() as usize;

            let bytes_read = cqe.result();

            if bytes_read < 0 {
                return Err(io::Error::from_raw_os_error(-bytes_read));
            }

            let buffer = &buffers[idx][..bytes_read as usize];

            let source = String::from_utf8_lossy(buffer).into_owned();

            results[idx] = Some(SourceFile {
                path: paths[idx].clone(),
                source,
            });
        }

        Ok(results.into_iter().map(Option::unwrap).collect())
    }
}
