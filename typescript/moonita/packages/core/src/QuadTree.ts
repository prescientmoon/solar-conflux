import { Adt } from "./Adt";
import { CircularBuffer } from "./CircularBuffer";
import { AABB, center, pointInside, rawPointInside } from "./game/common/AABB";
import { Vector2 } from "./game/common/Vector";
import { Pair } from "./Types";
import * as V from "./game/common/Vector";
import { TypedArray } from "wolf-ecs";
import { distanceSquared } from "./math";
import { FlexibleTypedArray } from "./FlexibleTypedArray";

export type Node = number;

const enum TreeKind {
  Leaf,
  Parent,
}

type NumberTypedArray = TypedArray & Record<number, number>;

type Positions = {
  x: NumberTypedArray;
  y: NumberTypedArray;
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

export interface QuadTreeSettings {
  maxNodes: number;
  positions: Positions;
  retriveInto: FlexibleTypedArray;
  entityMovementBuffer: FlexibleTypedArray;
}

// ========== Implementation
export class QuadTree {
  private nodes: Nodes;
  private readonly center: Vector2;

  public constructor(
    private readonly bounds: AABB,
    public readonly settings: QuadTreeSettings,
    private readonly depth = 0
  ) {
    this.center = center(this.bounds);

    // Trick ts into trusting us to initialize the prop here
    this.nodes = null as any;

    this.clear();
  }

  public moveEntities() {
    this.settings.entityMovementBuffer.clear();
    this.rawMoveEntities();
  }

  private rawMoveEntities() {
    const positions = this.settings.positions;

    if (this.nodes.type === TreeKind.Leaf) {
      for (let i = 0; i < this.nodes.nodes.used; i++) {
        const node = this.nodes.nodes.get(i)!;

        if (rawPointInside(this.bounds, positions.x[node], positions.y[node]))
          continue;
        else {
          this.settings.entityMovementBuffer.push(node);
          this.nodes.nodes.remove(i);
          i--;
        }
      }
    } else {
      const start = this.settings.entityMovementBuffer.used;
      for (let x = 0; x < 2; x++) {
        for (let y = 0; y < 2; y++) {
          const child = this.nodes.children![x][y];

          child.rawMoveEntities();
        }
      }

      const end = this.settings.entityMovementBuffer.used;
      this.settings.entityMovementBuffer.used = start;

      for (let i = start; i < end; i++) {
        const id = this.settings.entityMovementBuffer.elements[i];

        if (rawPointInside(this.bounds, positions.x[id], positions.y[id])) {
          this.assertInside(id);
          this.insert(id);
        } else this.settings.entityMovementBuffer.push(id);
      }

      // this.merge();
    }
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
      nodes: new CircularBuffer(this.settings.maxNodes),
      children: null,
    };
  }

  public retrieve(
    near: Vector2,
    radius: number,
    radiusSquared = radius * radius
  ) {
    this.settings.retriveInto.clear();
    return this.retrieveWithoutCleanup(near, radius, radiusSquared);
  }

  private retrieveWithoutCleanup(
    near: Vector2,
    radius: number,
    radiusSquared: number
  ) {
    if (this.nodes.type === TreeKind.Leaf) {
      for (let i = 0; i < this.nodes.nodes.used; i++) {
        const node = this.nodes.nodes.get(i)!;
        const x = this.settings.positions.x[node];
        const y = this.settings.positions.y[node];

        if (distanceSquared(x, y, near.x, near.y) <= radiusSquared) {
          this.settings.retriveInto.push(node);
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
            child.retrieveWithoutCleanup(near, radius, radiusSquared);
        }
      }
    }

    return this.settings.retriveInto;
  }

  public insert(node: Node) {
    const x = this.settings.positions.x[node];
    const y = this.settings.positions.y[node];

    if (!rawPointInside(this.bounds, x, y)) {
      debugger;
      throw new Error(`Node ${node} not inside quad tree bounds!!!`);
    }

    const nodes = this.nodes;
    if (nodes.type === TreeKind.Leaf) {
      if (this.nodes.nodes!.used === this.settings.maxNodes) {
        this.split();
        this.insert(node);
      } else {
        this.nodes.nodes!.tryPush(node);
      }
    } else if (nodes.type === TreeKind.Parent) {
      const a = Number(x > this.center.x);
      const b = Number(y > this.center.y);

      this.nodes.children![a][b].assertInside(node);
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

    if (length > this.settings.maxNodes) return;

    const children = this.nodes.children;

    this.clear();

    for (let x = 0; x < 2; x++) {
      for (let y = 0; y < 2; y++) {
        const child = children[x][y];

        for (let i = 0; i < child.nodes.nodes!.used; i++) {
          const id = child.nodes.nodes!.get(i)!;

          this.assertInside(id);
          this.insert(id);
        }
      }
    }
  }

  private assertInside(id: number) {
    if (
      !rawPointInside(
        this.bounds,
        this.settings.positions.x[id],
        this.settings.positions.y[id]
      )
    )
      debugger;
  }

  private split() {
    const size = V.scale(this.bounds.size, 1 / 2);

    this.nodes.children = [
      [
        new QuadTree(
          {
            position: this.bounds.position,
            size,
          },
          this.settings,
          this.depth + 1
        ),
        new QuadTree(
          {
            position: {
              x: this.bounds.position.x,
              y: this.bounds.position.y + size.y,
            },
            size,
          },
          this.settings,
          this.depth + 1
        ),
      ],
      [
        new QuadTree(
          {
            position: {
              x: this.bounds.position.x + size.x,
              y: this.bounds.position.y,
            },
            size,
          },
          this.settings,
          this.depth + 1
        ),
        new QuadTree(
          {
            position: {
              x: this.bounds.position.x + size.x,
              y: this.bounds.position.y + size.y,
            },
            size,
          },
          this.settings,
          this.depth + 1
        ),
      ],
    ];

    this.nodes.type = TreeKind.Parent;

    for (let i = 0; i < this.nodes.nodes!.used; i++) {
      const node = this.nodes.nodes!.get(i)!;

      this.assertInside(node);
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
