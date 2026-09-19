import ws from "ws";
import { Adt } from "./Adt";
import { Literal, Number, Record, Union } from "runtypes";

const settings = {
  timeoutAmount: 10000,
  heartbeatCycle: 1000,
};

const ConnectionPayload = Union(
  Record({
    type: Literal("reconnect"),
    value: Number,
  }),
  Record({
    type: Literal("connect"),
  })
);

export type WsClient = Adt<{
  connected: {
    socket: ws.WebSocket;
    confirmedAlive: boolean;
  };
  disconnected: {
    timeoutInterval: NodeJS.Timer;
  };
}>;

export type clientId = number;

export class WsHub {
  private nextId: clientId = 0;
  private clients = new Map<clientId, WsClient>();
  private heartbeatInterval: NodeJS.Timer;

  public dispose() {
    clearInterval(this.heartbeatInterval);
  }

  public constructor() {
    const server = new ws.Server({
      port: 8080,
    });

    server.on("connection", (socket, r) => {
      const init = () => {
        const clientId = this.nextId++;

        this.clients.set(clientId, {
          type: "connected",
          socket,
          confirmedAlive: true,
        });

        return clientId;
      };

      const setup = (clientId: clientId) => {
        socket.on("pong", () => {
          const client = this.clients.get(clientId);

          if (client?.type !== "connected") return;

          client.confirmedAlive = true;
        });
      };

      socket.once("message", (data) => {
        try {
          const json = JSON.parse(data.toString());
          const payload = ConnectionPayload.check(json);

          if (payload.type === "reconnect") {
            const client = this.clients.get(payload.value);
            if (client === null || client?.type === "connected") {
              // TODO: actual error messages
              return socket.terminate();
            }

            setup(payload.value);
          } else {
            const clientId = init();
            setup(clientId);
          }
        } catch {
          socket.terminate();
        }
      });
    });

    this.heartbeatInterval = setInterval(() => {
      for (const [id, client] of this.clients) {
        if (client.type === "disconnected") continue;

        if (client.confirmedAlive === false) this.disconnect(id);

        client.confirmedAlive = false;
        client.socket.ping();
      }
    }, settings.heartbeatCycle);
  }

  private disconnect(id: clientId, reconnectWindow = settings.timeoutAmount) {
    const client = this.clients.get(id);
    if (client?.type !== "connected") return;

    const timeoutInterval = setTimeout(() => {
      client.socket.terminate();
      this.clients.delete(id);
    }, reconnectWindow);

    this.clients.set(id, {
      type: "disconnected",
      timeoutInterval,
    });
  }
}
