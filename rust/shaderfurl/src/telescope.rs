/// A non-empty stack.
#[derive(Debug, Clone)]
pub struct Telescope<T> {
	elements: Vec<T>,
}

impl<T> Telescope<T> {
	pub fn new(v: T) -> Self {
		Self { elements: vec![v] }
	}

	pub fn tip(&self) -> &T {
		self.elements.last().unwrap()
	}

	pub fn focusing<O>(
		&mut self,
		element: T,
		compute: impl FnOnce(&mut Self) -> O,
	) -> O {
		self.elements.push(element);
		let output = compute(self);
		self.elements.pop();
		output
	}

	pub fn focusing_many<O>(
		&mut self,
		elements: impl IntoIterator<Item = T>,
		compute: impl FnOnce(&mut Self) -> O,
	) -> O {
		self.elements.extend(elements);
		let output = compute(self);
		self.elements.pop();
		output
	}
}

impl<T: PartialEq> Telescope<T> {
	/// Computes whether the telescope contains a closed cycle. If such a cycle
	/// exists, we could never escape it, thus it is guaranteed to be at the end
	/// of the telescope.
	pub fn find_cycle(&self) -> Option<&[T]> {
		let tip = self.tip();
		let rest = &self.elements[..self.elements.len() - 1];
		for (i, element) in rest.iter().enumerate().rev() {
			if element == tip {
				return Some(&rest[i..]);
			}
		}

		None
	}
}
