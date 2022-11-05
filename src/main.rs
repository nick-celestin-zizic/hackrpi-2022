use bevy::prelude::*;
use bevy::input::mouse::*;
use bevy::render::camera::*;
use bevy::render::render_resource::{SamplerDescriptor, FilterMode};
use bevy::render::texture::ImageSettings;

fn main() {
    App::new()
        .insert_resource(ImageSettings {
            default_sampler: SamplerDescriptor {
                mag_filter: FilterMode::Nearest,
                ..Default::default() }})
        .insert_resource(WindowDescriptor {
            width: 1067.0,
            height: 800.0,
            ..Default::default()
        })
        .add_plugins(DefaultPlugins)
        .add_startup_system(setup)
        .add_system(my_cursor_system)
        .run();
}

/// Used to help identify our main camera
#[derive(Component)]
struct MainCamera;

#[derive(Component)]
struct MainPlayer;

fn setup(mut commands: Commands, asset_server: Res<AssetServer>) {
    commands.spawn_bundle(Camera2dBundle::default()).insert(MainCamera);
    commands.spawn_bundle(SpriteBundle {
        texture: asset_server.load("littleguy.png"),
        transform: Transform::from_xyz(100., 0., 0.).with_scale(Vec3::new(5.0, 5.0, 5.0)),
        ..Default::default()
    }).insert(MainPlayer);
}

fn my_cursor_system(
    // need to get window dimensions
    wnds: Res<Windows>,
    // query to get camera transform
    q_camera: Query<(&Camera, &GlobalTransform), With<MainCamera>>,
    mut guy: Query<(&mut Transform,), With<MainPlayer>>
) {
    // get the camera info and transform
    // assuming there is exactly one main camera entity, so query::single() is OK
    let (camera, camera_transform) = q_camera.single();

    // get the window that the camera is displaying to (or the primary window)
    let wnd = if let RenderTarget::Window(id) = camera.target { wnds.get(id).unwrap() }
    else { wnds.get_primary().unwrap() };

    // check if the cursor is inside the window and get its position
    if let Some(screen_pos) = wnd.cursor_position() {
        // get the size of the window
        let window_size = Vec2::new(wnd.width() as f32, wnd.height() as f32);

        // convert screen position [0..resolution] to ndc [-1..1] (gpu coordinates)
        let ndc = (screen_pos / window_size) * 2.0 - Vec2::ONE;

        // matrix for undoing the projection and camera transform
        let ndc_to_world = camera_transform.compute_matrix() * camera.projection_matrix().inverse();

        // use it to convert ndc to world-space coordinates
        let world_pos = ndc_to_world.project_point3(ndc.extend(-1.0));

        // reduce it to a 2D value
        let world_pos: Vec2 = world_pos.truncate();

        for (mut transform,) in guy.iter_mut() {
            transform.translation = Vec3::new(world_pos.x, world_pos.y, 0.0);
        }
    }
}
