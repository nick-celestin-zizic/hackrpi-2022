use bevy::prelude::*;
use bevy::input::mouse::*;
use bevy::render::camera::*;
use bevy::input::*;
use bevy::input::keyboard::*;
use bevy::render::render_resource::{SamplerDescriptor, FilterMode};
use bevy::render::texture::ImageSettings;

mod rendering; use rendering::*;

const PLAYER_VEL:   f32 = 500.0;
const ROVER_SCALE: Vec3 = Vec3::new(2.5, 2.5, 2.5);
const CURSOR_SCALE: Vec3 = Vec3::new(3.0, 3.0, 3.0);

fn main() {
    App::new()
        .insert_resource(ImageSettings {
            default_sampler: SamplerDescriptor {
                mag_filter: FilterMode::Nearest,
                ..Default::default() }})
        .insert_resource(WindowDescriptor {
            width: 1337.0,
            height: 800.0,
            ..Default::default() })
        .add_plugins(DefaultPlugins)
        .add_startup_system(setup)
        .add_system(cursor_input_system)
        .add_system(player_input_system)
        .add_system(player_movement_system)
        .add_system(text_rendering_system)
        .run();
}

/// Used to help identify our main camera
#[derive(Component)]
struct MainCamera;

#[derive(Component, Default)]
struct Player {
    vel: Vec2,
}

#[derive(Component)]
struct Cursor;

fn setup(mut commands: Commands, asset_server: Res<AssetServer>) {
    commands.spawn_bundle(Camera2dBundle::default()).insert(MainCamera);
    
    commands.spawn_bundle(SpriteBundle {
        texture: asset_server.load("littleguy.png"),
        transform: Transform::from_xyz(100., 0., 0.).with_scale(ROVER_SCALE),
        ..Default::default()
    }).insert(Player::default());

    commands.spawn_bundle(SpriteBundle {
        texture: asset_server.load("cursor_1.png"),
        transform: Transform::from_xyz(100., 0., 0.).with_scale(CURSOR_SCALE),
        ..Default::default()
    }).insert(Cursor);

    
}

fn cursor_input_system(
    r_window: Res<Windows>,
    q_camera: Query<(&Camera, &GlobalTransform), With<MainCamera>>,
    mut q_cursor: Query<(&mut Transform,), With<Cursor>>,
) {
    // get the camera info and transform
    // assuming there is exactly one main camera entity, so query::single() is OK
    let (camera, camera_transform) = q_camera.single();
    

    // get the window that the camera is displaying to (or the primary window)
    let window = if let RenderTarget::Window(id) = camera.target { r_window.get(id).unwrap() }
    else { r_window.get_primary().unwrap() };

    // check if the cursor is inside the window and get its position
    if let Some(screen_pos) = window.cursor_position() {
        let window_size  = Vec2::new(window.width() as f32, window.height() as f32);
        let ndc          = (screen_pos / window_size) * 2.0 - Vec2::ONE;
        let ndc_to_world = camera_transform.compute_matrix() * camera.projection_matrix().inverse();
        let world_pos    = ndc_to_world.project_point3(ndc.extend(-1.0));

        let (mut transform,) = q_cursor.single_mut();
        transform.translation = Vec3::new(world_pos.x, world_pos.y, 0.0);
    }
}

fn player_input_system(
    mut e_key: EventReader<KeyboardInput>,
    mut q_player: Query<(&mut Player,)>
) {
    let (mut player,) = q_player.single_mut();

    //player.vel = default();
    
    for e in e_key.iter() { match e.state {
        ButtonState::Pressed => {
            match e.key_code {
                Some(KeyCode::W) => player.vel.y = PLAYER_VEL,
                Some(KeyCode::S) => player.vel.y = -PLAYER_VEL,
                Some(KeyCode::D) => player.vel.x = PLAYER_VEL,
                Some(KeyCode::A) => player.vel.x = -PLAYER_VEL,
                _ => {}
            }
        },
        ButtonState::Released => {
            match e.key_code {
                Some(KeyCode::W) => player.vel.y = 0.0,
                Some(KeyCode::S) => player.vel.y = 0.0,
                Some(KeyCode::D) => player.vel.x = 0.0,
                Some(KeyCode::A) => player.vel.x = 0.0,
                _ => {}
            }
        }
        _ => {}
    }}
}

fn player_movement_system(
    time: Res<Time>,
    mut q_player: Query<(&mut Transform, &Player)>
) {
    let (mut transform, player) = q_player.single_mut();

    transform.translation += Vec3::new(player.vel.x, player.vel.y, 0.0) * time.delta_seconds();
    
}
