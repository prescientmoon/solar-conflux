import {
  Intersect,
  Literal,
  Null,
  Number,
  Record,
  Runtype,
  String,
  Union,
  Unknown,
} from "runtypes";
import { Adt } from "./Adt";
import { Stream } from "./Stream";
import * as S from "./Stream";

type QuestionSchema = {
  type: string;
  input: unknown;
  output: unknown;
};

export type InputFor<Q extends QuestionSchema, K extends Q["type"]> = (Q & {
  type: K;
})["input"];
export type OutputFor<Q extends QuestionSchema, K extends Q["type"]> = (Q & {
  type: K;
})["output"];

export type QuestionValidators<
  Qin extends QuestionSchema,
  Qout extends QuestionSchema
> = {
  [K in Qout["type"]]: {
    output: Runtype<OutputFor<Qout, K>>;
  };
} & {
  [K in Qin["type"]]: {
    input: Runtype<InputFor<Qin, K>>;
  };
};

export type NotificationValidators<N> = Runtype<N>;

export type messageId = number;

export type Message<
  N,
  Qin extends QuestionSchema,
  Qout extends QuestionSchema
> = Adt<{
  notification: {
    data: N;
  };
  question: {
    data: Pick<Qout, "type"> & { data: Qout["input"] };
  };
  answer: {
    data: Pick<Qin, "type"> & { data: Qin["output"] };
    message: messageId;
  };
  parsingError: {
    message: messageId | null;
    error: string;
    data: null;
  };
}> &
  MessageMetadata;

export type MessageMetadata = { index: number; id: messageId };
export type WithMetadata<T> = MessageMetadata & { data: T };

export type UndecodedMessage = Omit<Message<any, any, any>, "data"> & {
  data: unknown;
};

const MessageMetadata: Runtype<MessageMetadata> = Record({
  index: Number,
  id: Number,
});

const Message: Runtype<UndecodedMessage> = Intersect(
  MessageMetadata,
  Union(
    Record({ type: Literal("notification"), data: Unknown }),
    Record({ type: Literal("question"), data: Unknown }),
    Record({
      type: Literal("answer"),
      data: Unknown,

      message: Number,
    }),
    Record({
      type: Literal("parsingError"),
      error: String,
      message: Union(Null, Number),
      data: Unknown,
    })
  )
);

export class CommunicationManager<
  Nin,
  Nout,
  Qin extends QuestionSchema,
  Qout extends QuestionSchema
> {
  private nextIndex = 0;
  private nextMessageId: messageId = 0;
  private unsawered = new Map<messageId, (v: UndecodedMessage) => void>();

  public notifications: Stream<WithMetadata<Nin>>;
  public parsingErrors: Stream<
    Message<Nin, Qout, Qin> & { type: "parsingError" }
  >;

  private cancellers = new Set<S.Effect<void>>();

  public dispose() {
    for (const canceller of this.cancellers) canceller();

    this.cancellers.clear();
  }

  public constructor(
    private notificationvalidators: NotificationValidators<Nin>,
    private questionValidators: QuestionValidators<Qin, Qout>,
    private incoming: Stream<string>,
    private outgoing: (v: string) => void
  ) {
    const [notification, emitNotification] = S.create<WithMetadata<Nin>>();
    const [parsingErrors, emitParsingError] = S.create<
      Message<Nin, Qout, Qin> & { type: "parsingError" }
    >();

    this.notifications = notification;
    this.parsingErrors = parsingErrors;

    const dispose = incoming((data) => {
      let perhapsIncomingId: null | messageId = null;

      try {
        const json = JSON.parse(data);
        const message = Message.check(json);

        // This can be sent back as extra info,
        // in case the notification parsing fails
        perhapsIncomingId = message.id;

        if (message.type === "notification") {
          const data = this.notificationvalidators.check(message.data);

          emitNotification({
            data,
            id: message.id,
            index: message.index,
          });
        } else if (message.type === "question") {
          // TODO: answer
        } else if (message.type === "answer") {
          const message_ = message as Message<Nin, Qout, Qin> & {
            type: "answer";
          };

          const callback = this.unsawered.get(message_.message);
          if (callback === null || callback === undefined) {
            // TODO: handle case where answer is confusing
            throw new Error(
              `Uh, now that's an answer I've never asked for... ${data}`
            );
          } else {
            // Delete callback from memory
            // I think calling resolve on a promise more than once
            // leads to an error
            this.unsawered.delete(message_.message);

            callback(message_);
          }
        } else if (message.type === "parsingError") {
          const message_ = message as Message<Nin, Qout, Qin> & {
            type: "parsingError";
          };

          // Handle case where a question couldn't be parsed
          // If we don't do this, some promise might hang forever,
          // waiting for an answer
          if (message_.message && this.unsawered.has(message_.message)) {
            const unsawered = this.unsawered.get(message_.message)!;

            unsawered(message);

            this.unsawered.delete(message_.message);
          } else {
            emitParsingError(message_);
          }
        }
      } catch (e) {
        const id = this.nextMessageId++;
        const message: Message<Nout, Qout, Qin> = {
          id,
          index: 0, // TODO: generate
          type: "parsingError",
          data: null,
          // TODO: check if this is the best way to do it
          error: (e as any).toString(),
          message: perhapsIncomingId,
        };

        outgoing(JSON.stringify(message));
      }
    });

    this.cancellers.add(dispose);
  }

  public notify(data: Nout): MessageMetadata {
    const id = this.nextMessageId++;
    const metadata: MessageMetadata = {
      id,
      index: 0, // TODO: this
    };

    const message: Message<Nout, Qin, Qout> = {
      ...metadata,
      data,
      type: "notification",
    };

    this.outgoing(JSON.stringify(message));

    return metadata;
  }

  public ask<K extends Qout["type"]>(
    key: K,
    data: InputFor<Qout, K>
  ): Promise<{ data: OutputFor<Qout, K> } & MessageMetadata> {
    let resolve = (_: UndecodedMessage): void => {
      throw new Error("Then called early");
    };

    const id = this.nextMessageId++;

    this.unsawered.set(id, (d) => resolve(d));

    return new Promise((resolve_, reject_) => {
      resolve = (raw: UndecodedMessage) => {
        this.unsawered.delete(id);

        try {
          const output = this.questionValidators[key].output.check(raw.data);

          resolve_({
            data: output,
            id: raw.id,
            index: raw.index,
          });
        } catch (e) {
          reject_({
            error: e,
            raw,
          });
        }
      };
    });
  }
}
