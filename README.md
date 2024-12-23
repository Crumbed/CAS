# CAS - Computer Algebra System
Compeletely unfinished and probably never will be but it IS semi-functional.

**How to Use** (as of now)
1. Make sure you have Rust installed [tutorial](https://www.rust-lang.org/learn/get-started)
2. Clone the github repo `git clone https://github.com/Crumbed/CAS`
3. Finally go to the project directory and run the command `cargo run`
4. You will be presented with a prompt `> ` where you can enter an expression
5. When you hit enter, the returned value of the expression will be displayed in the console
6. **As of now** when you enter something that causes an error, the entire program will crash, simply `cargo run` again
7. When you are done using the program simply type `.exit` in the prompt and the program will stop

**Currently Supports**
- Signed 64 bit integer(whole number) & floating point(decimal) data types, interchangeable
- Unary expressions, ex: `-2, x = -1, +x`
- Basic math operations `+, -, *, /`
- Exponents `x^y`
- Variables
- Implicit multiplication, ex: `2x, 2(1 + 2)`
- Basic functions, ex: `fn f(x, y) = x * y`, `f(2, 3)` ouputs: `6`

**In Progress**
- `let` keyword for solving for variables ex: `let -2x - 3 = 5x + 11` `x = -2`
- Block expressions using `{}` allowing multi-expression functions, ex:
  ```
  fn f(x) = {
    y = x * 2
    return x * y
  }
  ```
- Better error reporting
- Help command
