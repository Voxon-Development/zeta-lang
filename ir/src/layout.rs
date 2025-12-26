use crate::ssa_ir::SsaType;
use crate::hir::HirType;

#[derive(Clone, Copy, Debug)]
pub struct Layout { pub size: usize, pub align: usize }

#[derive(Debug)]
pub enum LayoutError {
    Unsized(&'static str),
    Recursive,
    Unknown,
}

#[derive(Clone, Copy)]
pub struct TargetInfo { pub ptr_bytes: u64 }

#[inline(always)]
const fn round_up(x: usize, align: usize) -> usize {
    if align <= 1 { return x; }
    let m = align - 1;
    (x + m) & !m
}

fn tag_bytes(variants: usize) -> usize {
    match variants {
        0..=0x100      => 1,
        0x101..=0x1_0000 => 2,
        0x1_0001..=0x1_0000_0000 => 4,
        _ => 8,
    }
}

pub fn sizeof_ssa(ty: &SsaType, target: TargetInfo) -> Result<usize, LayoutError> {
    Ok(layout_of_ssa(ty, target)?.size)
}

pub fn alignof_ssa(ty: &SsaType, target: TargetInfo) -> Result<usize, LayoutError> {
    Ok(layout_of_ssa(ty, target)?.align)
}

/// Calculate the size in bytes of a HIR type
pub fn sizeof_hir(ty: &HirType, target: TargetInfo) -> Result<usize, LayoutError> {
    Ok(layout_of_hir(ty, target)?.size)
}

/// Get the alignment requirement of a HIR type
pub fn alignof_hir(ty: &HirType, target: TargetInfo) -> Result<usize, LayoutError> {
    Ok(layout_of_hir(ty, target)?.align)
}

pub fn layout_of_ssa(ty: &SsaType, target: TargetInfo) -> Result<Layout, LayoutError> {
    match ty {
        SsaType::Void            => Ok(Layout { size: 0, align: 1 }),
        SsaType::Bool | SsaType::I8 | SsaType::U8  => Ok(Layout { size: 1, align: 1 }),
        SsaType::I16 | SsaType::U16                => Ok(Layout { size: 2, align: 2 }),
        SsaType::I32 | SsaType::U32 | SsaType::F32 => Ok(Layout { size: 4, align: 4 }),
        SsaType::I64 | SsaType::U64 | SsaType::F64 => Ok(Layout { size: 8, align: 8 }),

        SsaType::Slice | SsaType::Dyn => Err(LayoutError::Unsized("unsized type")),

        // Tuples/structs: sequential fields with padding between and at end to struct align.
        SsaType::Tuple(fields) | SsaType::User(_, fields) => {
            let mut off = 0usize;
            let mut max_align = 1usize;
            for fty in fields {
                let f: Layout = layout_of_ssa(fty, target)?;
                max_align = max_align.max(f.align);
                off = round_up(off, f.align);
                off = off.checked_add(f.size).ok_or(LayoutError::Unknown)?;
            }
            let size = round_up(off, max_align);
            Ok(Layout { size, align: max_align })
        }

        // Enums/sum types: Simple tagged union:
        SsaType::Enum(variants) => {
            if variants.is_empty() { return Ok(Layout { size: 0, align: 1 }); }
            let mut max_variant = Layout { size: 0, align: 1 };
            for v in variants {
                let l = layout_of_ssa(v, target)?;
                max_variant.size  = max_variant.size.max(l.size);
                max_variant.align = max_variant.align.max(l.align);
            }
            let tag = Layout { size: tag_bytes(variants.len()), align: 1 }; // define tag width
            let union_size = round_up(max_variant.size, max_variant.align);
            let total = round_up(union_size, tag.align).checked_add(tag.size).ok_or(LayoutError::Unknown)?;
            Ok(Layout { size: total, align: max_variant.align.max(tag.align) })
        }
        SsaType::I128 => Ok(Layout { size: 16, align: 16 }),
        SsaType::ISize => Ok(Layout { size: 8, align: 8 }),
        SsaType::USize => Ok(Layout { size: 8, align: 8 }),
        SsaType::String => Ok(Layout { size: 16, align: 16 }),
        SsaType::U128 => Ok(Layout { size: 16, align: 16 }),
        SsaType::Pointer(inner) => layout_of_ssa(inner, target),
        SsaType::Null => Ok(Layout { size: 0, align: 1 }),
    }
}

/// Calculate the memory layout of a HIR type
pub fn layout_of_hir(ty: &HirType, target: TargetInfo) -> Result<Layout, LayoutError> {
    match ty {
        // Primitive types
        HirType::I8 | HirType::U8 => Ok(Layout { size: 1, align: 1 }),
        HirType::I16 | HirType::U16 => Ok(Layout { size: 2, align: 2 }),
        HirType::I32 | HirType::U32 | HirType::F32 => Ok(Layout { size: 4, align: 4 }),
        HirType::I64 | HirType::U64 | HirType::F64 => Ok(Layout { size: 8, align: 8 }),
        HirType::I128 | HirType::U128 => Ok(Layout { size: 16, align: 16 }),
        
        // Special types
        HirType::Void | HirType::Null => Ok(Layout { size: 0, align: 1 }),
        HirType::Boolean => Ok(Layout { size: 1, align: 1 }),
        
        // String is a pointer + length (2 * ptr size)
        HirType::String => Ok(Layout { 
            size: (target.ptr_bytes * 2) as usize, 
            align: target.ptr_bytes as usize 
        }),
        
        // Pointers are always ptr_bytes in size
        HirType::Pointer(inner) => {
            let inner_layout = layout_of_hir(inner, target)?;
            Ok(Layout { 
                size: inner_layout.size,
                align: inner_layout.align
            })
        },
        
        // For class/enum/interface, we'll treat them as opaque pointers for now
        // In a full implementation, you would look up the actual type definition
        HirType::Struct(_, _) |
        HirType::Interface(_, _) | 
        HirType::Enum(_, _) => {
            Ok(Layout { 
                size: target.ptr_bytes as usize, 
                align: target.ptr_bytes as usize 
            })
        },
        
        HirType::Lambda { .. } => {
            Ok(Layout { 
                size: 0,
                align: 1
            })
        },

        HirType::Generic(_) => {
            Ok(Layout { 
                size: target.ptr_bytes as usize, 
                align: target.ptr_bytes as usize 
            })
        },
        
        // This/self pointer
        HirType::This => Ok(Layout { 
            size: target.ptr_bytes as usize, 
            align: target.ptr_bytes as usize 
        }),
    }
}