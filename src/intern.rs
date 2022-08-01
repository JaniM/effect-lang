use lasso::{Spur, ThreadedRodeo};
use once_cell::sync::Lazy;

pub static INTERNER: Lazy<ThreadedRodeo> = Lazy::new(ThreadedRodeo::new);

pub fn resolve_symbol(key: Spur) -> &'static str {
    INTERNER.resolve(&key)
}
