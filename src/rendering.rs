//use bevy::prelude::AssetServer;

//use bevy::text::Text2dSize;
use bevy::{prelude::*};

const DIMENSIONS:i32 = 25;
const UP:i32 = -1 * DIMENSIONS;
const DOWN:i32 = DIMENSIONS;
const RIGHT:i32 = 1;
const LEFT:i32 = -1;

#[derive(Component)]
pub struct EditorState
{
    pub pointer:i32,
    pub adder:i32,
    pub string_arr: Vec<char>,
}

//Input parameters for displaying text
//mut commands: Commands, asset_server: Res<AssetServer>
pub fn text_rendering_system(
    mut char_evr: EventReader<ReceivedCharacter>,
    mut q_editor: Query<&mut EditorState>,
    keys: Res<Input<KeyCode>>,
) {

    let mut ed = q_editor.single_mut();
    /* Display text
    //Wrapping box
    let font = asset_server.load("fonts/consola.ttf");
    let text_style = TextStyle
    {
        font,
        font_size: 30.0,
        color: Color::WHITE,
    };
    let box_size = Vec2::new(300.0, 200.0);
    commands.spawn_bundle(Text2dBundle {
        text: Text::from_section("This is text, and text is fun", text_style),
        text_2d_size: Text2dSize {
            size: box_size,
        },
        transform: Transform::from_xyz(0.0, -150.0, 1.0),
        ..default()
    });
    */

    // prints every char coming in; press enter to echo the full string

    if ed.string_arr.len() <= 0
    {
        //Add 1 for enter char
        ed.string_arr.resize((DIMENSIONS*DIMENSIONS) as usize, ' ');
    }

    for ev in char_evr.iter()
    {
        println!("Got char: '{}'", ev.char);
        match ev.char
        {
            'v' => {
                ed.adder=DOWN;
            },
            '^' => {
                ed.adder=UP;
            },
            '>' => {
                ed.adder=RIGHT;
            },
            '<' => {
                ed.adder=LEFT;
            },
            _ => {}
        }
        //Adds by 1 to ensure when 0 occures, it is at the end of the line
        let mut override_char:bool = false;
        if (((ed.pointer + 1) % DIMENSIONS == 0 && ed.adder == RIGHT) || ((ed.pointer + 1) % DIMENSIONS == 1 && ed.adder == LEFT))
        {
            override_char = true;
        }
        if((ed.pointer <= DIMENSIONS - 1 && ed.adder == UP) || (ed.pointer >= DIMENSIONS*DIMENSIONS - (DIMENSIONS + 1) && ed.adder == DOWN))
        {
            override_char = true;
        }
        if (ed.pointer + ed.adder < 0 && ed.pointer + ed.adder > DIMENSIONS*DIMENSIONS)
        {
            if ed.string_arr[(ed.pointer + ed.adder) as usize] == '\0'
            {
                override_char = true;
            }
        }
        let print_pointer = ed.pointer;
        let print_adder = ed.adder;
        println!("{print_pointer} += {print_adder}: {override_char}");
        let index: usize = ed.pointer as usize;
        if ev.char != '\r'
        {
            ed.string_arr[index] = ev.char;
        }
        if !override_char && ev.char != '\r'
        {
            ed.pointer += ed.adder;
        }
    }

    if keys.just_pressed(KeyCode::Return)
    {
        let mut string: String = String::new();
        let mut temp: String = String::new();
        for i in 0..(DIMENSIONS*(DIMENSIONS + 1)) as usize
        {
            string.push(ed.string_arr[i]);
            temp.push(ed.string_arr[i]);
            if ((i + 1) as i32) % (DIMENSIONS + 1) == 0
            {
                string.push('\n');
                temp.push('\n');
            }
        }
        println!("{}", temp);
        ed.pointer = 0;
        ed.adder = 1;
        ed.string_arr = Vec::new();
    }
}
