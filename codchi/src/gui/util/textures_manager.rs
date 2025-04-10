use egui::*;
use image::{ImageBuffer, ImageReader, Rgba};
use std::{
    collections::HashMap,
    path::{Path, PathBuf},
    sync::mpsc::{channel, Receiver, Sender},
    thread,
};

pub struct TexturesManager {
    textures: HashMap<String, TextureHandle>,

    sender: Sender<ChannelDTO>,
    receiver: Receiver<ChannelDTO>,
    awaits_answer: bool,
}

enum ChannelDTO {
    ColorImage(String, ColorImage),
    Image(String, ImageBuffer<Rgba<u8>, Vec<u8>>, [usize; 2]),
}

impl TexturesManager {
    pub fn new() -> Self {
        let (sender, receiver) = channel();
        load_square_textures_async(&sender);
        Self {
            textures: HashMap::new(),

            sender,
            receiver,
            awaits_answer: false,
        }
    }

    pub fn update(&mut self, ctx: &Context) {
        self.receive_msgs(ctx);
    }

    pub fn get(&self, name: &str) -> Option<&TextureHandle> {
        self.textures.get(name)
    }

    pub fn get_image(&self, name: &str) -> Option<Image> {
        self.get(name).map(|tex| Image::from_texture(tex))
    }

    pub fn deliver(&mut self, name: &str, path: &str, dim: u32) -> Option<&TextureHandle> {
        let texture_handle = self.textures.get(name);

        if !self.awaits_answer && texture_handle.is_none() {
            self.awaits_answer = true;
            load_color_image_async(&self.sender, name.to_string(), path.to_string(), dim);
        }

        texture_handle
    }

    pub fn deliver_image(&mut self, name: &str, path: &PathBuf) -> Option<&TextureHandle> {
        let texture_handle = self.textures.get(name);

        if !self.awaits_answer && texture_handle.is_none() {
            self.awaits_answer = true;
            load_image_async(&self.sender, name.to_string(), path.clone());
        }

        texture_handle
    }

    fn receive_msgs(&mut self, ctx: &Context) {
        while let Ok(channel_dto) = self.receiver.try_recv() {
            self.awaits_answer = false;
            match channel_dto {
                ChannelDTO::ColorImage(name, color_image) => {
                    let texture_handle = ctx.load_texture(&name, color_image, Default::default());
                    self.textures.insert(name, texture_handle);
                }
                ChannelDTO::Image(name, image_buffer, size) => {
                    let texture_handle = ctx.load_texture(
                        "icon_texture",
                        ColorImage::from_rgba_unmultiplied(size, &image_buffer),
                        Default::default(),
                    );
                    self.textures.insert(name, texture_handle);
                }
            }
        }
    }
}

fn load_image_async(sender: &Sender<ChannelDTO>, name: String, path: PathBuf) {
    let sender_clone = sender.clone();
    thread::spawn(move || {
        if let Some((image_buffer, size)) = load_image_from_path(&path) {
            let dto = ChannelDTO::Image(name, image_buffer, size);
            sender_clone.send(dto).unwrap();
        }
    });
}

fn load_image_from_path(path: &PathBuf) -> Option<(ImageBuffer<Rgba<u8>, Vec<u8>>, [usize; 2])> {
    let image = image::open(path)
        .expect("Failed to open image icon")
        .to_rgba8();
    let size = [image.width() as usize, image.height() as usize];

    Some((image, size))
}

fn load_color_image_async(sender: &Sender<ChannelDTO>, name: String, path: String, dim: u32) {
    let sender_clone = sender.clone();
    thread::spawn(move || {
        if let Some(color_image) = load_color_image_from_path(&path, dim) {
            let dto = ChannelDTO::ColorImage(name, color_image);
            sender_clone.send(dto).unwrap();
        }
    });
}

fn load_color_image_from_path(path: &str, dim: u32) -> Option<ColorImage> {
    let img = ImageReader::open(Path::new(path))
        .ok()?
        .decode()
        .ok()?
        .resize(dim, dim, image::imageops::FilterType::Lanczos3)
        .to_rgba8();

    let (width, height) = img.dimensions();
    let pixels = img.into_raw();

    Some(ColorImage::from_rgba_unmultiplied(
        [width as usize, height as usize],
        &pixels,
    ))
}

fn load_square_textures_async(sender: &Sender<ChannelDTO>) {
    let sender_clone = sender.clone();
    thread::spawn(move || {
        let dto = ChannelDTO::ColorImage(
            "black".to_string(),
            ColorImage::new([10, 10], Color32::BLACK),
        );
        sender_clone.send(dto).unwrap();
    });

    let sender_clone = sender.clone();
    thread::spawn(move || {
        let dto = ChannelDTO::ColorImage(
            "dark_red".to_string(),
            ColorImage::new([10, 10], Color32::DARK_RED),
        );
        sender_clone.send(dto).unwrap();
    });

    let sender_clone = sender.clone();
    thread::spawn(move || {
        let dto =
            ChannelDTO::ColorImage("red".to_string(), ColorImage::new([10, 10], Color32::RED));
        sender_clone.send(dto).unwrap();
    });

    let sender_clone = sender.clone();
    thread::spawn(move || {
        let dto = ChannelDTO::ColorImage(
            "green".to_string(),
            ColorImage::new([10, 10], Color32::GREEN),
        );
        sender_clone.send(dto).unwrap();
    });
}
