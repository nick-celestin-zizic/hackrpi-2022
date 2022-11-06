//use bevy::prelude::AssetServer;

//use bevy::text::Text2dSize;
use bevy::{prelude::*};

const DIMENSIONS:i8 = 10;
const UP:i8 = DIMENSIONS;
const DOWN:i8 = -1 * DIMENSIONS;
const RIGHT:i8 = 1;
const LEFT:i8 = -1;

//Input parameters for displaying text
//mut commands: Commands, asset_server: Res<AssetServer>
pub fn text_rendering_system(mut char_evr: EventReader<ReceivedCharacter>, keys: Res<Input<KeyCode>>, mut string: Local<String>)
{
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

    let mut pointer:i8 = 0;
    for i in 0..(DIMENSIONS*DIMENSIONS)
    {
        string.push(' ');
    }

    let stringArr: Vec<char> = string.chars().collect();
    string.clear();

    for ev in char_evr.iter()
    {
        //println!("Got char: '{}'", ev.char);
        pointer += 1;
        /*
        match ev.char
        {
            'v' => {
                
            }
        }
        */
    }

    if keys.just_pressed(KeyCode::Return)
    {
        let arr: Vec<char> = string.chars().collect();
        println!("Got char at index 2: '{}'", arr[2]);
        println!("Text input: {}", *string);
        string.clear();
    }
}
