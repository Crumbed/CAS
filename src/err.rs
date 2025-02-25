use std::fmt;




pub type RtResult<T> = std::result::Result<T, RuntimeError>;

#[derive(Debug, Clone)]
pub enum RuntimeError {
    VarUndefined(String),
    FnUndefined(String, usize),
    InvalidFnDec(Vec<String>),
    SolveFor(String)
}
use RuntimeError as Rt;


impl fmt::Display for RuntimeError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", match self {
            Rt::VarUndefined(id) => format!("Undefined variable: {id}"),
            Rt::FnUndefined(id, p_count) => format!("Undefined function: {id} with {p_count} parameters"),
            Rt::InvalidFnDec(found) =>
                format!("Invalid function signature! Expected identifiers but found {:?}", found),
            Rt::SolveFor(msg) => msg.to_string(),
        })
    }
}



