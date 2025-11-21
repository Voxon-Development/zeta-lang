use zetaruntime::string_pool::StringPool;

pub struct Context<'a> {
    pub string_pool: &'a mut StringPool
}