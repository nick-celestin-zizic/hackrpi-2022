//use bevy::prelude::AssetServer;

//use bevy::text::Text2dSize;
use bevy::{prelude::*};

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
    for ev in char_evr.iter()
    {
        println!("Got char: '{}'", ev.char);
        string.push(ev.char);
    }

    if keys.just_pressed(KeyCode::Return)
    {
        println!("Text input: {}", *string);
        string.clear();
    }
}
