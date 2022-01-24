import { Adt } from "./Adt";
import { CircularBuffer } from "./CircularBuffer";
import { AABB, center, pointInside, rawPointInside } from "./game/common/AABB";
import { Vector2 } from "./game/common/Vector";
import { Pair } from "./Types";
import * as V from "./game/common/Vector";
import { TypedArray } from "wolf-ecs";

// TODO: don't store the position here
export interface Node {
  position: Vector2;
  id: number;
}

const enum TreeKind {
  Leaf,
  Parent,
}

type Positions = {
  x: TypedArray;
  y: TypedArray;
};

type Nodes = Adt<{
  [TreeKind.Leaf]: {
    children: null;
    nodes: CircularBuffer<Node>;
  };
  [TreeKind.Parent]: {
    nodes: null;
    children: Pair<Pair<QuadTree>>;
  };
}>;

export class QuadTree {
  private nodes: Nodes;
  private center: Vector2;

  public constructor(
    private maxNodes: number,
    private bounds: AABB,
    private depth = 0
  ) {
    this.center = center(this.bounds);

    // Trick ts into trusting us to initialize the prop here
    this.nodes = null as any;

    this.clear();
  }

  public moveEntities(positions: Positions): number[] {
    const problematic = new Array();
    if (this.nodes.type === TreeKind.Leaf) {
      for (let i = 0; i < this.nodes.nodes.used; i++) {
        const node = this.nodes.nodes.get(i)!;

        // Skip unchanged positions
        if (
          positions.x[node.id] === node.position.x &&
          positions.y[node.id] === node.position.y
        )
          continue;

        if (
          rawPointInside(
            this.bounds,
            positions.x[node.id] as number,
            positions.y[node.id] as number
          )
        ) {
          node.position.x = positions.x[node.id] as number;
          node.position.y = positions.y[node.id] as number;
        } else {
          problematic.push(node.id);
          this.nodes.nodes.remove(i);
          i--;
        }
      }
    } else {
      for (let x = 0; x < 2; x++) {
        for (let y = 0; y < 2; y++) {
          const child = this.nodes.children![x][y];

          const problems = child.moveEntities(positions);

          for (let i = 0; i < problems.length; i++) {
            const id = problems[i];

            if (
              rawPointInside(
                this.bounds,
                positions.x[id] as number,
                positions.y[id] as number
              )
            ) {
              this.insert({
                id,
                position: {
                  x: positions.x[id] as number,
                  y: positions.y[id] as number,
                },
              });
            } else problematic.push(id);
          }
        }
      }

      // this.merge();
    }

    return problematic;
  }

  public cleanup() {
    if (this.nodes.type === TreeKind.Leaf) return;

    for (let x = 0; x < 2; x++) {
      for (let y = 0; y < 2; y++) {
        this.nodes.children[x][y].cleanup();
      }
    }

    this.merge();
  }

  public clear() {
    this.nodes = {
      type: TreeKind.Leaf,
      nodes: new CircularBuffer(this.maxNodes),
      children: null,
    };
  }

  public retrieve(
    near: Vector2,
    radius: number,
    process: (node: Node) => void,
    radiusSquared = radius * radius
  ) {
    if (this.nodes.type === TreeKind.Leaf) {
      for (let i = 0; i < this.nodes.nodes.used; i++) {
        const node = this.nodes.nodes.get(i)!;

        if (V.distanceSquared(node.position, near) <= radiusSquared) {
          process(node);
        }
      }
    } else {
      // TODO: take a look at https://gamedev.stackexchange.com/questions/96337/collision-between-aabb-and-circle
      // Right now this isn't that efficient (it can lead to redundant collision checks)

      const childRadius =
        (Math.max(
          this.nodes.children[0][0].bounds.size.x,
          this.nodes.children[0][0].bounds.size.y
        ) *
          Math.SQRT1_2 +
          radius) **
        2;

      for (let x = 0; x < 2; x++) {
        for (let y = 0; y < 2; y++) {
          const child = this.nodes.children![x][y];

          if (V.distanceSquared(near, child.center) <= childRadius)
            child.retrieve(near, radius, process, radiusSquared);
        }
      }
    }
  }

  public insert(node: Node) {
    if (!pointInside(this.bounds, node.position)) {
      debugger;
      throw new Error(`Node ${node.id} not inside quad tree bounds!!!`);
    }

    const nodes = this.nodes;
    if (nodes.type === TreeKind.Leaf) {
      if (this.nodes.nodes!.used === this.maxNodes) {
        this.split();
        this.insert(node);
      } else {
        this.nodes.nodes!.tryPush(node);
      }
    } else if (nodes.type === TreeKind.Parent) {
      const a = Number(node.position.x > this.center.x);
      const b = Number(node.position.y > this.center.y);

      this.nodes.children![a][b].insert(node);
    }
  }

  private merge() {
    if (this.nodes.type === TreeKind.Leaf) return;

    let length = 0;
    for (let x = 0; x < 2; x++) {
      for (let y = 0; y < 2; y++) {
        const child = this.nodes.children[x][y];

        if (child.nodes.type === TreeKind.Parent) return;

        length += child.nodes.nodes.used;
      }
    }

    if (length > this.maxNodes) return;

    const children = this.nodes.children;

    this.clear();

    for (let x = 0; x < 2; x++) {
      for (let y = 0; y < 2; y++) {
        const child = children[x][y];

        for (let i = 0; i < child.nodes.nodes!.used; i++) {
          this.insert(child.nodes.nodes!.get(i)!);
        }
      }
    }
  }

  private split() {
    const size = V.scale(this.bounds.size, 1 / 2);

    this.nodes.children = [
      [
        new QuadTree(
          this.maxNodes,
          {
            position: this.bounds.position,
            size,
          },
          this.depth + 1
        ),
        new QuadTree(
          this.maxNodes,
          {
            position: {
              x: this.bounds.position.x,
              y: this.bounds.position.y + size.y,
            },
            size,
          },
          this.depth + 1
        ),
      ],
      [
        new QuadTree(
          this.maxNodes,
          {
            position: {
              x: this.bounds.position.x + size.x,
              y: this.bounds.position.y,
            },
            size,
          },
          this.depth + 1
        ),
        new QuadTree(
          this.maxNodes,
          {
            position: {
              x: this.bounds.position.x + size.x,
              y: this.bounds.position.y + size.y,
            },
            size,
          },
          this.depth + 1
        ),
      ],
    ];

    this.nodes.type = TreeKind.Parent;

    for (let i = 0; i < this.nodes.nodes!.used; i++) {
      const node = this.nodes.nodes!.get(i)!;

      this.insert(node);
    }

    this.nodes.nodes = null;
  }

  public render(ctx: CanvasRenderingContext2D) {
    if (this.nodes.type === TreeKind.Leaf) return;

    ctx.strokeStyle = "blue";
    ctx.lineWidth = 1;
    ctx.beginPath();
    ctx.moveTo(this.center.x, this.bounds.position.y);
    ctx.lineTo(this.center.x, this.bounds.position.y + this.bounds.size.y);
    ctx.moveTo(this.bounds.position.x, this.center.y);
    ctx.lineTo(this.bounds.position.x + this.bounds.size.x, this.center.y);
    ctx.stroke();

    for (let x = 0; x < 2; x++) {
      for (let y = 0; y < 2; y++) {
        this.nodes.children![x][y].render(ctx);
      }
    }
  }
}
