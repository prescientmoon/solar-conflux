use std::collections::HashMap;
use std::fmt::Debug;
use std::hash::Hash;

// {{{ Types
/// Trait for items to expose their dependencies, used for topological sorting.
pub trait Dependencies<K> {
	fn dependencies(&self) -> Vec<K>;
}

/// Node states for the DFS traversal.
#[derive(Debug, PartialEq, Eq, Clone, Copy)]
enum NodeState {
	Unvisited,    // Not yet visited
	Visiting,     // Currently being visited
	VisitedEarly, // Visited too early — added a cycle breaker instead
	Visited,      // Fully processed
}

/// Represents a node in the final sorted list, indicating if it's a regular
/// element or a cycle breaker.
///
/// Cycle breakers are artificial elements that are added in to "fake" a node.
/// They are treated as that node by subsequent elements, yet they don't depend
/// on any of the original node's dependencies. Think of them like function
/// declarations (without an actual body) in a C-like language (jeez, I wonder
/// why we would need such functionality for GLSL :p).
#[derive(Debug, PartialEq, Eq, Copy, Clone)]
pub enum NodeStatus<K> {
	Regular(K),
	CycleBreaker(K),
}

impl<K> NodeStatus<K> {
	/// Returns a reference to the inner key, regardless of its status.
	pub fn key(&self) -> &K {
		match self {
			NodeStatus::Regular(k) => k,
			NodeStatus::CycleBreaker(k) => k,
		}
	}
}
// }}}
// {{{ Entrypoint
/// Implements a generic topological sort algorithm that handles cycles and
/// explicitly marks cycle breakers (check [NodeStatus] for details of what that
/// means).
///
/// This implementation does not try to minimize the number of cycle breakers
/// in any way. The cycle breaker count is guaranteed to be at most `N-1`,
/// where `N` is the size of the given graph.
pub fn topological_sort<K, V>(graph: &HashMap<K, V>) -> Vec<NodeStatus<K>>
where
	K: Eq + Hash + Clone,
	V: Dependencies<K>,
{
	if graph.is_empty() {
		return Vec::new();
	}

	// We preallocate the maximum possible size, to prevent further
	// pre-allocations.
	let mut sorted_list = Vec::with_capacity(2 * graph.len() - 1);
	let mut states = HashMap::with_capacity(graph.len());

	// Iterate over all nodes. This ensures all disconnected components
	// of the graph are processed.
	for key in graph.keys() {
		if matches!(states.get(key), None | Some(NodeState::Unvisited)) {
			dfs_visit(key, graph, &mut states, &mut sorted_list);
		}
	}

	assert_eq!(sorted_list.len(), graph.len());

	// Add any remaining nodes that broke cycles to the final array.
	//
	// We know this is safe because every node must appear in the final array
	// precisely once by this point (either as a breaker or as a regular node).
	for key in graph.keys() {
		if matches!(states.get(key), Some(NodeState::VisitedEarly)) {
			sorted_list.push(NodeStatus::Regular(key.clone()));
		}
	}

	sorted_list
}
// }}}
// {{{ DFS
/// Recursive DFS helper function to visit nodes and build the sorted list.
fn dfs_visit<K, V>(
	node_key: &K,
	graph: &HashMap<K, V>,
	states: &mut HashMap<K, NodeState>,
	sorted_list: &mut Vec<NodeStatus<K>>,
) where
	K: Eq + Hash + Clone,
	V: Dependencies<K>,
{
	let state = states.get(node_key).unwrap_or(&NodeState::Unvisited);
	assert!(matches!(state, NodeState::Unvisited));
	states.insert(node_key.clone(), NodeState::Visiting);

	if let Some(node_value) = graph.get(node_key) {
		for dep_key in node_value.dependencies() {
			// Only consider dependencies that actually exist in the graph.
			// (this should always be the case in practice, but oh well)
			if !graph.contains_key(&dep_key) {
				continue;
			}

			match states.get(&dep_key).unwrap_or(&NodeState::Unvisited) {
				// Dependency not yet visited, recurse on it.
				NodeState::Unvisited => dfs_visit(&dep_key, graph, states, sorted_list),

				// Oops, we've found a cycle! Mark this as a breaker and move on.
				//
				// We return right away, as iterating over any more dependencies might
				// introduce needless cycle breakers to the list. That would still not
				// break the agreed upon bound of `2N - 1` elements, but we have no
				// reason to do it.
				NodeState::Visiting => {
					sorted_list.push(NodeStatus::CycleBreaker(node_key.clone()));
					states.insert(node_key.clone(), NodeState::VisitedEarly);
					return;
				}

				// Dependency already visited, do nothing.
				NodeState::VisitedEarly | NodeState::Visited => {}
			}
		}
	}

	sorted_list.push(NodeStatus::Regular(node_key.clone()));
	states.insert(node_key.clone(), NodeState::Visited);
}
// }}}

// {{{ Tests
#[cfg(test)]
mod tests {
	use super::*;
	use NodeStatus::*;
	use std::collections::HashMap;

	// A simple struct to represent an item in the graph with dependencies.
	struct Task(&'static [usize]);

	impl Dependencies<usize> for Task {
		fn dependencies(&self) -> Vec<usize> {
			self.0.to_vec()
		}
	}

	fn assert_valid_sort<K: Eq, V: Dependencies<K>>(
		graph: &HashMap<K, V>,
		sorted: &[NodeStatus<K>],
	) {
		for (k, v) in graph.iter() {
			let pos = sorted
				.iter()
				.enumerate()
				.find_map(|(i, node)| match node {
					Regular(node) if node == k => Some(i),
					_ => None,
				})
				.unwrap();

			for dep in v.dependencies() {
				let found_dep = sorted.iter().take(pos).any(|node| node.key() == &dep);
				assert!(found_dep);
			}
		}
	}

	#[test]
	fn test_empty_graph() {
		let graph: HashMap<usize, Task> = HashMap::new();
		let sorted = topological_sort(&graph);
		assert!(sorted.is_empty());
	}

	#[test]
	fn test_single_node() {
		let mut graph = HashMap::new();
		graph.insert(0, Task(&[]));
		let sorted = topological_sort(&graph);
		assert_valid_sort(&graph, &sorted);
	}

	#[test]
	fn test_linear_dependency() {
		// C -> B -> A
		let mut graph = HashMap::new();
		graph.insert(0, Task(&[1]));
		graph.insert(1, Task(&[2]));
		graph.insert(2, Task(&[]));

		let sorted = topological_sort(&graph);
		assert_valid_sort(&graph, &sorted);
	}

	#[test]
	fn test_multiple_dependencies() {
		// D -> B -> A
		// D -> C -> A
		let mut graph = HashMap::new();
		graph.insert(0, Task(&[1, 2]));
		graph.insert(1, Task(&[3]));
		graph.insert(2, Task(&[3]));
		graph.insert(3, Task(&[]));
		let sorted = topological_sort(&graph);
		assert_valid_sort(&graph, &sorted);
	}

	#[test]
	fn test_graph_with_direct_cycle() {
		// A -> B -> A
		let mut graph = HashMap::new();
		graph.insert(0, Task(&[1]));
		graph.insert(1, Task(&[0]));

		let sorted = topological_sort(&graph);
		assert_valid_sort(&graph, &sorted);
	}

	#[test]
	fn test_graph_with_longer_cycle() {
		// A -> B -> C -> A
		let mut graph = HashMap::new();
		graph.insert(0, Task(&[1]));
		graph.insert(1, Task(&[2]));
		graph.insert(2, Task(&[0]));

		let sorted = topological_sort(&graph);
		assert_valid_sort(&graph, &sorted);
	}

	#[test]
	fn test_graph_with_triple_cycle() {
		// A -> B -> A
		// B -> C -> B
		// C -> A -> C
		let mut graph = HashMap::new();
		graph.insert(0, Task(&[1, 2]));
		graph.insert(1, Task(&[2, 1]));
		graph.insert(2, Task(&[0, 1]));

		let sorted = topological_sort(&graph);
		assert_valid_sort(&graph, &sorted);
	}

	#[test]
	fn test_disconnected_components() {
		// A -> B and X -> Y
		let mut graph = HashMap::new();
		graph.insert(0, Task(&[1]));
		graph.insert(1, Task(&[]));
		graph.insert(2, Task(&[]));
		graph.insert(3, Task(&[2]));

		let sorted = topological_sort(&graph);
		assert_valid_sort(&graph, &sorted);
	}
}
// }}}
