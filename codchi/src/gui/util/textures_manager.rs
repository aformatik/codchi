use anyhow::Result;
use egui::*;
use image::{ImageBuffer, ImageReader, Rgba};
use resvg::tiny_skia::Pixmap;
use std::{
    collections::{HashMap, HashSet},
    path::PathBuf,
    sync::mpsc::{channel, Receiver, Sender},
    thread,
};

pub struct TexturesManager {
    textures: HashMap<String, TextureHandle>,

    sender: Sender<(String, ChannelDTO)>,
    receiver: Receiver<(String, ChannelDTO)>,

    open_requests: HashSet<String>,
}

enum ChannelDTO {
    ColorImage(ColorImage),
}

impl TexturesManager {
    pub fn new() -> Self {
        let (sender, receiver) = channel();
        load_square_textures_async(&sender);
        Self {
            textures: HashMap::new(),

            sender,
            receiver,

            open_requests: HashSet::new(),
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

    pub fn deliver(&mut self, name: &str, path: &str) -> Option<&TextureHandle> {
        let texture_handle = self.textures.get(name);

        if texture_handle.is_none() {
            let name_string = name.to_string();
            if !self.open_requests.contains(&name_string) {
                self.open_requests.insert(name_string);
                load_color_image_async(&self.sender, name.to_string(), path.to_string());
            }
        }

        texture_handle
    }

    pub fn deliver_ico(&mut self, name: &str, path: &PathBuf) -> Option<&TextureHandle> {
        let texture_handle = self.textures.get(name);

        if texture_handle.is_none() {
            let name_string = name.to_string();
            if !self.open_requests.contains(&name_string) {
                self.open_requests.insert(name_string);
                load_image_buffer_async(&self.sender, name.to_string(), path.clone());
            }
        }

        texture_handle
    }

    pub fn deliver_svg(&mut self, name: &str, path: &str) -> Option<&TextureHandle> {
        let texture_handle = self.textures.get(name);

        if texture_handle.is_none() {
            let name_string = name.to_string();
            if !self.open_requests.contains(&name_string) {
                self.open_requests.insert(name_string);
                load_svg_async(&self.sender, name.to_string(), path.to_string());
            }
        }

        texture_handle
    }

    fn receive_msgs(&mut self, ctx: &Context) {
        while let Ok((name, channel_dto)) = self.receiver.try_recv() {
            self.open_requests.remove(&name);
            match channel_dto {
                ChannelDTO::ColorImage(color_image) => {
                    let texture_handle = ctx.load_texture(&name, color_image, Default::default());
                    self.textures.insert(name, texture_handle);
                }
            }
        }
    }
}

fn load_color_image_async(sender: &Sender<(String, ChannelDTO)>, name: String, location: String) {
    let sender_clone = sender.clone();
    thread::spawn(move || {
        let path = PathBuf::from(&location);
        if let Ok(color_image) = load_color_image_from_path(&path) {
            let dto = ChannelDTO::ColorImage(color_image);
            sender_clone.send((name, dto)).unwrap();
        }
    });
}

fn load_image_buffer_async(sender: &Sender<(String, ChannelDTO)>, name: String, path: PathBuf) {
    let sender_clone = sender.clone();
    thread::spawn(move || {
        if let Ok((image_buffer, size)) = load_image_from_path(&path) {
            let color_image = ColorImage::from_rgba_unmultiplied(size, &image_buffer);
            let dto = ChannelDTO::ColorImage(color_image);
            sender_clone.send((name, dto)).unwrap();
        }
    });
}

fn load_svg_async(sender: &Sender<(String, ChannelDTO)>, name: String, location: String) {
    let sender_clone = sender.clone();
    thread::spawn(move || {
        let path = PathBuf::from(&location);
        if let Ok(color_image) = load_svg_from_path(&path) {
            let dto = ChannelDTO::ColorImage(color_image);
            sender_clone.send((name, dto)).unwrap();
        }
    });
}

fn load_color_image_from_path(path: &PathBuf) -> Result<ColorImage> {
    let image = ImageReader::open(path)?.decode()?.to_rgba8();

    let (width, height) = image.dimensions();
    let pixels = image.into_raw();

    Ok(ColorImage::from_rgba_unmultiplied(
        [width as usize, height as usize],
        &pixels,
    ))
}

fn load_image_from_path(path: &PathBuf) -> Result<(ImageBuffer<Rgba<u8>, Vec<u8>>, [usize; 2])> {
    let image = image::open(path)?.to_rgba8();
    let size = [image.width() as usize, image.height() as usize];

    Ok((image, size))
}

fn load_svg_from_path(path: &PathBuf) -> Result<ColorImage> {
    let rtree = usvg::Tree::from_data(&std::fs::read(path)?, &usvg::Options::default())?;

    let w = rtree.size().width();
    let h = rtree.size().height();

    let mut pixmap = Pixmap::new(w as u32, h as u32).unwrap();
    resvg::render(&rtree, Default::default(), &mut pixmap.as_mut());

    Ok(ColorImage::from_rgba_unmultiplied(
        [w as usize, h as usize],
        pixmap.data(),
    ))
}

fn load_square_textures_async(sender: &Sender<(String, ChannelDTO)>) {
    let sender_clone = sender.clone();
    thread::spawn(move || {
        let dto = ChannelDTO::ColorImage(ColorImage::new([10, 10], Color32::BLACK));
        sender_clone.send(("black".to_string(), dto)).unwrap();
    });

    let sender_clone = sender.clone();
    thread::spawn(move || {
        let dto = ChannelDTO::ColorImage(ColorImage::new([10, 10], Color32::DARK_RED));
        sender_clone.send(("dark_red".to_string(), dto)).unwrap();
    });

    let sender_clone = sender.clone();
    thread::spawn(move || {
        let dto = ChannelDTO::ColorImage(ColorImage::new([10, 10], Color32::RED));
        sender_clone.send(("red".to_string(), dto)).unwrap();
    });

    let sender_clone = sender.clone();
    thread::spawn(move || {
        let dto = ChannelDTO::ColorImage(ColorImage::new([10, 10], Color32::GREEN));
        sender_clone.send(("green".to_string(), dto)).unwrap();
    });
}
