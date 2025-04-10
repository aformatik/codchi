pub(crate) struct StatusEntries {
    entries: Vec<Option<StatusEntry>>,
    free_entries: Vec<usize>,
}

impl StatusEntries {
    pub fn new() -> Self {
        Self {
            entries: Vec::new(),
            free_entries: Vec::new(),
        }
    }

    pub fn create_entry(&mut self, text: String) -> usize {
        self.push(text, 1)
    }

    pub fn push(&mut self, text: String, durability: usize) -> usize {
        self.push_entry(StatusEntry { text, durability })
    }

    fn push_entry(&mut self, status_entry: StatusEntry) -> usize {
        if let Some(free_index) = self.free_entries.pop() {
            self.entries[free_index] = Some(status_entry);
            free_index
        } else {
            self.entries.push(Some(status_entry));
            self.entries.len() - 1
        }
    }

    pub fn decrease(&mut self, index: usize) {
        if index < self.entries.len() {
            if let Some(status_entry) = self.entries[index].as_mut() {
                if status_entry.decrement() == 0 {
                    self.entries[index] = None;
                    self.free_entries.push(index);
                }
            }
        }
    }

    pub fn get_status(&self) -> Vec<&str> {
        let mut result = Vec::new();
        for status_entry in &self.entries {
            if let Some(entry) = status_entry {
                result.push(entry.get_text());
            }
        }
        result
    }
}

struct StatusEntry {
    text: String,
    durability: usize,
}

impl StatusEntry {
    fn decrement(&mut self) -> usize {
        self.durability = self.durability.saturating_sub(1);
        self.durability
    }

    fn get_text(&self) -> &str {
        &self.text
    }
}
