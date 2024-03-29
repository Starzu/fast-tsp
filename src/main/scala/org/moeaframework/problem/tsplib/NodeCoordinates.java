/* Copyright 2012 David Hadka
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to
 * deal in the Software without restriction, including without limitation the
 * rights to use, copy, modify, merge, publish, distribute, sublicense, and/or
 * sell copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
 * FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS
 * IN THE SOFTWARE.
 */
package org.moeaframework.problem.tsplib;

import java.io.BufferedReader;
import java.io.IOException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * Stores the nodes in a TSPLIB problem instance and provides methods for
 * calculating the distances between nodes.
 */
public class NodeCoordinates extends DistanceTable {

	/**
	 * The number of nodes to load into this problem instance.
	 */
	private final int size;

	/**
	 * The type of coordinates, used to ensure the TSPLIB problem instance is
	 * parsed correctly.
	 */
	private final NodeCoordType type;

	/**
	 * The distance function.
	 */
	private final DistanceFunction distanceFunction;

	/**
	 * The mapping from identifiers to nodes.
	 */
	private final Node[] nodes;

	/**
	 * Constructs a new, empty node coordinates instance.
	 *
	 * @param size the number of nodes to load into this problem instance
	 * @param edgeWeightType the edge weight type
	 */
	public NodeCoordinates(int size, EdgeWeightType edgeWeightType) {
		this(size, edgeWeightType.getNodeCoordType(),
				edgeWeightType.getDistanceFunction());
	}

	/**
	 * Constructs a new, empty node coordinates instance.
	 *
	 * @param size the number of nodes to load into this problem instance
	 * @param type the type of coordinates (i.e., 2D or 3D)
	 * @param distanceFunction the distance function
	 */
	public NodeCoordinates(int size, NodeCoordType type,
			DistanceFunction distanceFunction) {
		super();
		this.size = size;
		this.type = type;
		this.distanceFunction = distanceFunction;

		nodes = new Node[size + 1];
	}

	@Override
	public void load(BufferedReader reader) throws IOException {
		for (int i = 0; i < size; i++) {
			String line = reader.readLine();
			String[] tokens = line.trim().split("\\s+");

			if (tokens.length != type.getLength() + 1) {
				throw new IOException(
						"invalid number of tokens for node entry");
			}

			double[] position = new double[type.getLength()];
			int id = Integer.parseInt(tokens[0]);

			for (int j = 0; j < type.getLength(); j++) {
				position[j] = Double.parseDouble(tokens[j+1]);
			}

			add(new Node(id, position));
		}
	}

	/**
	 * Adds the specified node to this problem instance.  If a node with the
	 * same identifier already exists, the previous node will be replaced.
	 *
	 * @param node the node to add
	 */
	protected void add(Node node) {
		nodes[node.getId()] = node;
	}

	/**
	 * Returns the node with the specified identifier.
	 *
	 * @param id the identifier of the node to return
	 * @return the node with the specified identifier
	 */
	public Node get(int id) {
		return nodes[id];
	}

	/**
	 * Removes the node with the specified identifier from this problem
	 * instance.
	 *
	 * @param id the identifier of the node to remove
	 */
	protected void remove(int id) {
		nodes[id] = null;
	}

	/**
	 * Removes all nodes from this problem instance.
	 */
	protected void clear() {
//		nodes.clear();
	}

	/**
	 * Returns the number of nodes that this instance contains.
	 *
	 * @return the number of nodes that this instance contains
	 */
	public int size() {
		return nodes.length;
	}

	@Override
	public int[] listNodes() {
		int index = 0;
		int[] result = new int[size];

		for (Node node : nodes) {
			result[index++] = node.getId();
		}

		return result;
	}

	@Override
	public boolean isNeighbor(int id1, int id2) {
		return true;
	}

	@Override
	public List<Integer> getNeighborsOf(int id) {
		if ((id < 1) || (id > size)) {
			throw new IllegalArgumentException("no node with identifier " + id);
		}

		List<Integer> neighbors = new ArrayList<>(size - 1);
		for (int i = 1; i <= size; i++) {
			if (i != id) {
				neighbors.add(i);
			}
		}

		return neighbors;
	}

	@Override
	public double getDistanceBetween(int id1, int id2) {
		Node node1 = get(id1);
		Node node2 = get(id2);

		if (node1 == null) {
			throw new IllegalArgumentException("no node with identifier " +
					id1);
		}

		if (node2 == null) {
			throw new IllegalArgumentException("no node with identifier " +
					id2);
		}

		return distanceFunction.distance(get(id1), get(id2));
	}

	@Override
	public String toString() {
		StringBuilder sb = new StringBuilder();

		for (Node node : nodes) {
			sb.append(node.toString());
			sb.append('\n');
		}

		return sb.toString();
	}

}
