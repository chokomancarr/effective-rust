use rich_phantoms::PhantomCovariantAlwaysSendSync;

pub mod eff;
pub mod channel;

pub enum Zero {}
pub struct Succ<T>(PhantomCovariantAlwaysSendSync<T>);

