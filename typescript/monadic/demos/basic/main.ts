import {
  runUi,
  withAction,
  mkChild,
  buildSpec,
  withState,
  emptySpec,
  withTemplate,
  withChild,
  withOutput,
} from '../../src';
import { render, html, TemplateResult } from 'lit-html';
import { pipe } from 'fp-ts/lib/pipeable';
import { constant } from 'fp-ts/es6/function';
import * as O from 'fp-ts/es6/Option';

const baseSpec = pipe(emptySpec, withTemplate<TemplateResult>());

// Todo component
type TodoState = {
  name: string;
  done: boolean;
};

type TodoAction = 'complete';

type TodoOutput = 'delete';

const todo = pipe(
  baseSpec,
  withAction<TodoAction>(),
  withState<TodoState>(),
  withOutput<TodoOutput>(),
  buildSpec(
    ({ name, done }, { dispatch, raise }) => {
      return html`
        <div>
          Name: ${name} Completed: ${done}
          <button @click=${() => dispatch('complete')}>Complete todo</button>
          <button @click=${() => raise('delete')}>Delete todo</button>
        </div>
      `;
    },
    (action, state) => {
      if (action === 'complete') {
        return {
          ...state,
          done: true,
        };
      }

      return state;
    },
    {}
  )
);

// Todo list input component
type TodoListInputAction =
  | {
      label: 'setInput';
      value: string;
    }
  | 'create';

type TodoListInputOutput = 'createTodo';

const todoListInput = pipe(
  baseSpec,
  withAction<TodoListInputAction>(),
  withState<string>(),
  withOutput<TodoListInputOutput>(),
  buildSpec(
    (value, { dispatch }) => {
      return html`
        <input
          value=${value}
          @input=${(event: InputEvent) => {
            const target = event.target as HTMLInputElement;
            dispatch({ label: 'setInput', value: target.value });
          }}
        />
        <button @click=${() => dispatch('create')}>Add todo</button>
      `;
    },
    (action, state, { raise }) => {
      if (action === 'create') {
        raise('createTodo');
        return state;
      }

      return action.value;
    },
    {}
  )
);

// Todo list component
type TodoListState = {
  todos: TodoState[];
  inputValue: string;
};

type TodoListAction = 'createTodo' | { label: 'deleteTodo'; index: number };

const todoList = pipe(
  baseSpec,
  withAction<TodoListAction>(),
  withState<TodoListState>(),
  withChild<'todo', number, TodoState, null>(),
  withChild<'input', null, string, TodoListInputOutput>(),
  buildSpec(
    (state, { child }) => {
      return html`
        <div>
          ${child('input', 'input', null)}
          <div>
            ${state.todos.map((_, index) =>
              child('todo', String(index), index)
            )}
          </div>
        </div>
      `;
    },
    (action, state) => {
      if (action === 'createTodo') {
        return {
          inputValue: '',
          todos: [
            ...state.todos,
            {
              name: state.inputValue,
              done: false,
            },
          ],
        };
      } else if (action.label === 'deleteTodo') {
        return {
          ...state,
          todos: state.todos.filter((_, index) => index !== action.index),
        };
      }
    },
    {
      todo: mkChild(
        index => ({
          get: ({ todos }) => todos[index],
          set: (state, newTodoState) => ({
            ...state,
            todos: state.todos.map((todoState, currentIndex) =>
              index === currentIndex ? newTodoState : todoState
            ),
          }),
        }),
        todo,
        (index, output) =>
          output === 'delete'
            ? O.some({ label: 'deleteTodo', index } as TodoListAction)
            : O.none
      ),
      input: mkChild(
        constant({
          get: ({ inputValue }) => inputValue,
          set: (state, newInputValue) => ({
            ...state,
            inputValue: newInputValue,
          }),
        }),
        todoListInput,
        constant(O.some('createTodo' as TodoListAction))
      ),
    }
  )
);

const main = async () => {
  const output = runUi({
    parent: document.getElementById('app'),
    render,
    initialState: { todos: [], inputValue: '' },
    component: todoList,
  });

  for await (const event of output) {
    console.log('Received an event from the todo list component!');
  }
};

main().catch(err => {
  console.log('An error ocurred while running the root component');
  throw err;
});
