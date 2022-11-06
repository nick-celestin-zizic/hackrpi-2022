pub enum Pd
{
    UP, DOWN, LEFT, RIGHT
}

pub struct puzzel
{
    template: String,
    solution: String,
}

pub enum Mode
{
    NORMAL, READNUM, READSTRING
}

const WIDTH: usize = 100;

pub struct BefungeState
{
    pc: usize,
    pd: Pd,
    stack: Vec <i32>,
    mode: Mode,
    stdout: Vec <String>,
    // program: Vec<BefungeValue>,
}

fn eval (strr: &str, mut state: BefungeState) -> BefungeState
{
    let max = WIDTH;

    let str: String = strr.split('\n').flat_map(|x| {
        let d = max - x.len();
        let mut padding = Vec::new();

        // kreten glupi....
        for c in x.chars() { padding.push(c);   }
        for _ in 0..d      { padding.push(' '); }

        padding
    }).collect();

    let stack = &mut state.stack;

    let mut program: Vec<char> =
        str.chars().filter(|x| !(*x == '\n')).collect();

    let mut buffer = Vec::new();

    let mut offset = WIDTH;

    while program[state.pc] != '@'
    {

        println!("{:?}", stack);

        match state.mode
        {
            Mode::READNUM => {
                match program[state.pc]
                {
                    _ if '0' <= program[state.pc] && program[state.pc] <= '9'
                        => buffer.push(program[state.pc]),
                    _ =>
                    {
                        let x: i32 = buffer.into_iter().collect::<String>().parse().unwrap();
                        stack.push(x);
                        buffer = Vec::new();
                        state.mode = Mode::NORMAL;
                    }
                };
            }

            Mode::READSTRING => {
                match program[state.pc]
                {
                    _  if !('"' == program[state.pc])
                        => stack.push(program[state.pc] as i32),
                    _ => {
                        state.mode = Mode::NORMAL;
                    }
                };
            }

            Mode::NORMAL =>

        match program[state.pc]
        {
            '+' => {
                let a = stack.pop().unwrap_or(0);
                let b = stack.pop().unwrap_or(0);
                stack.push(a + b);
            },
            '-' => {
                let a = stack.pop().unwrap_or(0);
                let b = stack.pop().unwrap_or(0);
                stack.push(b - a);
            },
            '/' => {
                let a = stack.pop().unwrap_or(0);
                let b = stack.pop().unwrap_or(0);
                stack.push(b / a);
            },
            '*' => {
                let a = stack.pop().unwrap_or(0);
                let b = stack.pop().unwrap_or(0);
                stack.push(a * b);
            },
            '%' => {
                let a = stack.pop().unwrap_or(0);
                let b = stack.pop().unwrap_or(0);
                stack.push(b % a);
            },
            'v' => {
                state.pd = Pd::DOWN;
            },
            '^' => {
                state.pd = Pd::UP;
            },
            '>' => {
                state.pd = Pd::RIGHT;
            },
            '<' => {
                state.pd = Pd::LEFT;
            },
            // '?' => {
            //     let x = rng.gen<u32>() % 4;


            //     state.pd = match x
            //     {
            //         0 => Pd::UP,
            //         1 => Pd::DOWN,
            //         2 => Pd::LEFT,
            //         3 => Pd::LEFT,
            //     }
            // }
            '!' => {
                let a = stack.pop().unwrap_or(0);

                stack.push
                    (match a
                     {
                         0 => 1,
                         _ => 0,
                     });
            }
            '`' => {
                let a = stack.pop().unwrap_or(0);
                let b = stack.pop().unwrap_or(0);

                stack.push
                    (match b > a
                     {
                         true => 1,
                         false => 0,
                     });
            }

            '_' => {
                let a = stack.pop().unwrap_or(0);

                match a
                {
                    0 => state.pd = Pd::RIGHT,
                    _ => state.pd = Pd::LEFT
                }
            }

            '|' => {
                let a = stack.pop().unwrap_or(0);

                match a
                {
                    0 => state.pd = Pd::DOWN,
                    _ => state.pd = Pd::UP
                }

            }

            ':' => {
                let a = stack.pop().unwrap_or(0);
                stack.push(a);
                stack.push(a);
            }

            '\\' => {
                let a = stack.pop().unwrap_or(0);
                let b = stack.pop().unwrap_or(0);
                stack.push(a);
                stack.push(b);
            }
            '.' => {
                let a = stack.pop().unwrap_or(0);
                state.stdout.push(format!("{}\n", a));
            }
            ',' => {
                let a = stack.pop().unwrap_or(0);
                state.stdout.push(format!("{}\n", unsafe {std::char::from_u32_unchecked(a as u32)}));
            }
            'p' => {
                let y = stack.pop().unwrap_or(0);
                let x = stack.pop().unwrap_or(0);
                let v = stack.pop().unwrap_or(0);

                let datum = unsafe {std::char::from_u32_unchecked(v as u32)};

                let offset1: usize = x as usize + (WIDTH * y as usize) as usize;

                program[offset1] = datum;
            }
            'g' => {
                let y = stack.pop().unwrap_or(0);
                let x = stack.pop().unwrap_or(0);

                let offset1: usize = x as usize + (WIDTH * y as usize) as usize;
                let datum = unsafe {std::char::from_u32_unchecked(program[offset1] as u32)};

                stack.push(datum as i32);

            }
            // LMAO/
            '"' =>  state.mode = Mode::READSTRING,
            _ if '0' <= program[state.pc] && program[state.pc] <= '9'
                => {
                    buffer.push(program[state.pc]); state.mode = Mode::READNUM},
            _ => {}
        }
        }

        match state.pd
        {
            Pd::RIGHT => state.pc = (state.pc + 1) % offset,
            Pd::LEFT => state.pc = (state.pc - 1) % offset,
            Pd::DOWN => {
                offset += WIDTH;
                state.pc = (state.pc + WIDTH) % offset
            },
            Pd::UP => {
                offset -= WIDTH;
                state.pc = (state.pc - WIDTH) % offset},
        }

    }

    return state;

}

fn main ()
{

    let mut state = BefungeState {
        pc: 0,
        pd: Pd::RIGHT,
        stack: Vec::new(),
        mode: Mode::NORMAL,
        stdout: Vec::new(),
    };

    println!("{:?}",
             eval("2 5 *                :v
                   v:,_@
                   >  ^",
                 state).stdout)
}
