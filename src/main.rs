use bevy::prelude::*;
use bevy::render::camera::*;
use bevy::input::*;
use bevy::input::keyboard::*;
use bevy::render::render_resource::*;
use bevy::render::texture::*;

mod rendering; use rendering::*;

const PLAYER_VEL:   f32 = 75.0;
const ROVER_SCALE:  Vec3 = Vec3::new(1.5, 1.5, 1.5);
const CURSOR_SCALE: Vec3 = Vec3::new(3.0, 3.0, 3.0);

#[derive(Component)]
struct MainCamera;

#[derive(Component, Default)]
struct Player {
    vel: Vec2,
}

#[derive(Component)]
struct Cursor;

#[derive(Component, Deref, DerefMut)]
struct AnimationTimer(Timer);

#[derive(Component)]
struct Rover {
    fixed: bool
}

#[derive(Component)]
enum GameMode {
    Roving, Coding
}

#[derive(Component)]
struct EditRectangle;

#[derive(Component)]
struct Bg;

fn main() {
    App::new()
        .insert_resource(ImageSettings {
            default_sampler: SamplerDescriptor {
                mag_filter: FilterMode::Nearest,
                ..Default::default() }})
        .insert_resource(WindowDescriptor {
            width: 640.0,
            height: 384.0,
            ..Default::default() })
        .add_plugins(DefaultPlugins)
        .add_startup_system(setup)
        .add_startup_system(start_background_audio)
        .add_system(player_input_system)
        .add_system(player_movement_system)
        .add_system(player_animation_system)
        .add_system(text_rendering_system)
        .add_system(rover_fix_system_entry)
        .add_system(coding_system)
        .add_system(rover_fix_system_exit)
        .run();
}
// 0.7

fn start_background_audio(asset_server: Res<AssetServer>, audio: Res<Audio>) {
    audio.play(asset_server.load("ambient-space-music.ogg"));
}

fn setup(
    mut commands: Commands,
    asset_server: Res<AssetServer>,
    mut texture_atlases: ResMut<Assets<TextureAtlas>>,
) {
    let main_anim_handle       = asset_server.load("little_guy_anim.png");
    let main_anim_atlas        = TextureAtlas::from_grid(main_anim_handle, Vec2::new(32.0, 32.0), 40, 1);
    let main_anim_atlas_handle = texture_atlases.add(main_anim_atlas);
    
    commands.spawn_bundle( Camera2dBundle {
        transform: Transform::from_scale(Vec3::new(1.0, 1.0, 1.0)).with_translation(Vec3::new(0.0, 0.0, 3.0)),
        ..Camera2dBundle::default()
    }).insert(MainCamera);

    commands.spawn().insert(GameMode::Roving);

    
    commands.spawn_bundle(SpriteBundle {
        texture: asset_server.load("map.png"),
        transform: Transform::from_xyz(0.0, 0.0, 0.0).with_scale(Vec3::new(1., 1., 0.0)),
        ..Default::default()
    }).insert(Bg);
    
    commands.spawn_bundle(SpriteSheetBundle {
        texture_atlas: main_anim_atlas_handle,
        transform: Transform::from_xyz(-250., 150., 1.).with_scale(ROVER_SCALE),
        ..Default::default()
    }).insert(Player::default()).insert(AnimationTimer(Timer::from_seconds(0.02, true)));

    commands.spawn_bundle(SpriteBundle {
        texture: asset_server.load("r0ger.png"),
        transform: Transform::from_xyz(280., -140., 1.).with_scale(ROVER_SCALE),
        ..Default::default()
    }).insert(Rover {fixed: false});

    commands.spawn_bundle(SpriteBundle {
        texture: asset_server.load("citrus.png"),
        transform: Transform::from_xyz(250., 100., 1.).with_scale(ROVER_SCALE),
        ..Default::default()
    }).insert(Rover {fixed: false});

    commands.spawn_bundle(SpriteBundle {
        texture: asset_server.load("penolope.png"),
        transform: Transform::from_xyz(-250., -150., 1.).with_scale(ROVER_SCALE),
        ..Default::default()
    }).insert(Rover {fixed: false});

    commands.spawn_bundle(SpriteBundle {
        texture: asset_server.load("timmy.png"),
        transform: Transform::from_xyz(-20., 130., 1.).with_scale(ROVER_SCALE),
        ..Default::default()
    }).insert(Rover {fixed: false});

    commands.spawn().insert(EditorState {pointer: 0, adder: 1, string_arr: Vec::new()});


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
                Some(KeyCode::W) => player.vel.y = if player.vel.y > 0.0 { 0.0 } else { player.vel.y },
                Some(KeyCode::S) => player.vel.y = if player.vel.y < 0.0 { 0.0 } else { player.vel.y },
                Some(KeyCode::D) => player.vel.x = if player.vel.x > 0.0 { 0.0 } else { player.vel.x },
                Some(KeyCode::A) => player.vel.x = if player.vel.x < 0.0 { 0.0 } else { player.vel.x },
                _ => {}
            }
        }
    }}
}

fn player_movement_system(
    time: Res<Time>,
    mut q_player: Query<(&mut Transform, &Player)>
) {
    let (mut transform, player) = q_player.single_mut();
    transform.translation += Vec3::new(player.vel.x, player.vel.y, 0.0) * time.delta_seconds();
}

fn player_animation_system(
    time: Res<Time>,
    texture_atlases: Res<Assets<TextureAtlas>>,
    mut query: Query<(
        &mut AnimationTimer,
        &mut TextureAtlasSprite,
        &Handle<TextureAtlas>,)>,
) {
    for (mut timer, mut sprite, texture_atlas_handle) in &mut query {
        timer.tick(time.delta());
        if timer.just_finished() {
            let texture_atlas = texture_atlases.get(texture_atlas_handle).unwrap();
            sprite.index = (sprite.index + 1) % texture_atlas.textures.len();
        }
    } 
}

const ROVER_RANGE: f32 = 60.0;

fn coding_system(
    mut q_player: Query<(&Transform, &Player)>,
    mut q_game_mode: Query<(&mut GameMode)>,
    mut commands: Commands
){
    let (game_mode) = q_game_mode.single();
    if let GameMode::Coding = game_mode {
        commands.spawn_bundle(SpriteBundle {
            sprite: Sprite { color: (Color::BLACK), flip_x: false, flip_y: false, custom_size: Some(Vec2::new(360., 160.)), anchor: (bevy::sprite::Anchor::Center) },
            transform: Transform::from_xyz(0., 0., 3.).with_scale(ROVER_SCALE),
            ..Default::default()
        }).insert(EditRectangle);
    }
}

fn rover_fix_system_entry(
    keys: Res<Input<KeyCode>>,
    time: Res<Time>,
    mut q_player: Query<(&Transform, &Player)>,
    mut q_game_mode: Query<(&mut GameMode)>,
    mut q_rovers: Query<(&Transform, &Rover, Entity)>
){
    if keys.just_released(KeyCode::Space) {
        
        let (player_transform, player) = q_player.single();
        let mut min_x = 9999.;
        let mut min_y = 9999.;
        let mut closest : Entity;
        for (transform, rover, entity) in &q_rovers {
            if !rover.fixed {
                let x_diff = (transform.translation.x - player_transform.translation.x).abs();
                let y_diff = (transform.translation.y - player_transform.translation.y).abs();
                if x_diff <= ROVER_RANGE && x_diff < min_x && y_diff <= ROVER_RANGE && y_diff < min_y {
                    min_x = x_diff;
                    min_y = y_diff;
                    closest = entity;
                }
            }
        }
        if min_x <= ROVER_RANGE && min_y <= ROVER_RANGE {
            let mut gam = q_game_mode.single_mut();
            *gam = GameMode::Coding;
            
        }
    }
    
}

fn rover_fix_system_exit(
    keys: Res<Input<KeyCode>>,
    time: Res<Time>,
    mut q_player: Query<(&Transform, &Player, Without<EditRectangle>)>,
    mut q_game_mode: Query<(&mut GameMode)>,
    mut q_rovers: Query<(&Transform, &Rover, Entity, Without<EditRectangle>)>,
    mut q_rectangle: Query<&mut Transform, With<EditRectangle>>,
    //Without<Transform>>,
    mut commands: Commands
)
{
    let (game_mode) = q_game_mode.single();
    if let GameMode::Coding = game_mode {
        if keys.just_released(KeyCode::Escape) {
            // let mut transform = q_rectangle.single_mut();
            //transform.translation.z = -1.;
            dbg!("hello");
        }
    }
}
