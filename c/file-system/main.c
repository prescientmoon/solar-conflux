#include <stdlib.h>
#include <stdio.h>
#include <stdbool.h>
#include <string.h>
#define  MAX_PATH_LENGTH 60
#define  MAX_COMMAND_LENGTH 256

typedef struct {
  char path[MAX_PATH_LENGTH];
} Path;

typedef enum CommandType {
  EXIT,
  LS,
  CD,
  CAT,
  TOUCH,
  ECHO_INTO,
  // Not required, but useful for debugging. Prints the current working directory.
  PWD
} CommandType;

typedef struct Command Command;

struct Command {
  CommandType type;
  union {
    struct { Path path; } cd;
    struct { Path path; } ls;
    struct { Path path; } cat;
    struct { Path path; char contents[MAX_COMMAND_LENGTH]; bool overwrite; } echo;
    struct { char filenames[MAX_COMMAND_LENGTH]; } touch;
  } data;
};

typedef struct {
  char* contents;
  size_t contents_cap;
  size_t contents_len; // The file could theoretically be large, so `strlen` could be slow.

  // We use reference counting to maintain hardlinks
  int ref_count;
} FileData;

typedef struct Node Node;

// I'm prepending `T_` to prevent naming conflicts
typedef enum {
  T_Directory,
  T_File
} NodeType;

typedef struct Directory {
  Node* children;
  size_t children_count;
  size_t children_cap;
} Directory;

struct Node {
  char name[MAX_PATH_LENGTH];
  Node* parent;
  NodeType type;
  union {
    Directory directory;
    FileData* file;
  } data;
};

typedef struct State {
  Node root;
  Path cwd;
} State;

Path str_path(char *str) {
  Path out;
  strcpy(out.path, str);
  return out;
}

Path parse_path(char args[MAX_COMMAND_LENGTH], Path default_path) {
  Path out;

  if (sscanf(args, "%s", out.path)) {
    return out;
  } else {
    return default_path;
  }
}

Command parse_command(State *state) {
  char command[MAX_COMMAND_LENGTH];
  char args[MAX_COMMAND_LENGTH];
  Command out = {0};

  int count = scanf("%s%[^\n]", command, args);
  if (count >= 1) {
    // Skip space
    if (count == 2) {
      sscanf(args, " %[^\n]", args);
    } else {
      strcpy(args, "");
    }

    if (strcmp(command, "exit") == 0) {
      out.type = EXIT;
    } else if (strcmp(command, "pwd") == 0) {
      out.type = PWD;
    } else if (strcmp(command, "ls") == 0) {
      out.type = LS;
      out.data.ls.path = parse_path(args, state->cwd);
    } else if (strcmp(command, "cat") == 0) {
      out.type = CAT;
      out.data.ls.path = parse_path(args, str_path(""));
    } else if (strcmp(command, "touch") == 0) {
      out.type = TOUCH;
      strcpy(out.data.touch.filenames, args);
    } else if (strcmp(command, "cd") == 0) {
      out.type = CD;
      out.data.cd.path = parse_path(args, str_path("/"));
    } else if (strcmp(command, "echo") == 0) {
      char operator[3];
      char contents[MAX_COMMAND_LENGTH];
      if (sscanf(
        args,
        "\"%[^\"]\" %[>] %[^\n]",
        contents,
        operator,
        args
      ) == 3) {
        out.type = ECHO_INTO;
        if (strcmp(operator, ">") == 0) {
          out.data.echo.overwrite = true;
        } else if (strcmp(operator, ">>") == 0) {
          out.data.echo.overwrite = false;
        } else {
          printf("ERROR: unknown operator '%s' after `echo` command\n", operator);
        }

        out.data.echo.path = parse_path(args, str_path(""));
        strcpy(out.data.echo.contents, contents);
      }
    }
  }

  return out;
}

Path make_absolute(State *state, Path path) {
  if (path.path[0] == '/') { return path; }

  Path out = state->cwd;
  strcat(out.path, path.path);
  return out;
}

Node* get_node_by_path(Node* root, Path path) {
  if (strcmp(path.path, "") == 0) {
    return root;
  } else if (strcmp(path.path, ".") == 0) {
    return root;
  } else if (strcmp(path.path, "..") == 0) {
    return root->parent;
  }

  // Before and after the slash
  Path pre, post = {0};

  char *first_slash = strchr(path.path, '/');
  bool found_slash = first_slash != NULL;

  if (found_slash) {
    int slash_index = (int)(first_slash - &path.path[0]);
    strncpy(pre.path, path.path, slash_index);
    pre.path[slash_index] = '\0';
    strcpy(post.path, &path.path[slash_index + 1]);
  } else {
    pre = path;
  }

  // TODO: we could use binary search here 🤔
  for (int i = 0; i < root->data.directory.children_count; i++) {
    Node *child = &root->data.directory.children[i];

    if (strcmp(child->name, pre.path) == 0) {
      if (found_slash) {
        return get_node_by_path(child, post);
      } else {
        return child;
      }
    }
  }

  printf("ERROR: could not find file '%s', split into '%s' and '%s'\n", path.path, pre.path, post.path);
  return root;
}

// Returns `true` when the program should quit.
bool exec_command(State *state, Command command) {
  if (command.type == EXIT) {
    return true;
  } else if (command.type == PWD) {
    printf("%s\n", state->cwd.path);
  } else if (command.type == LS) {
    Node *node = get_node_by_path(
      &state->root,
      make_absolute(state, command.data.ls.path)
    );

    for (int i = 0; i < node->data.directory.children_count; i++) {
      Node *child = &node->data.directory.children[i];
      printf("%s\n", child->name);
    }
  } else if (command.type == CAT) {
    // If no path was provided, we print nothing
    if (command.data.cat.path.path[0] == '\0') { return false; }

    Node *node = get_node_by_path(
      &state->root,
      make_absolute(state, command.data.cat.path)
    );
    printf("%s\n", node->data.file->contents);
  } else if (command.type == CD) {
    strcpy(state->cwd.path, command.data.cd.path.path);
  } else if (command.type == TOUCH) {
    Node *node = get_node_by_path(&state->root, state->cwd);
    char args[MAX_COMMAND_LENGTH];
    strcpy(args, command.data.touch.filenames);
    char* filename = strtok(args, " ");

    while (filename != NULL) {
      Directory *directory = &node->data.directory;
      int target_index = directory->children_count;

      // TODO: use binary search, this is inefficient af
      for (int i = 0; i < directory->children_count; i++) {
        Node *child = &directory->children[i];
        if (strcmp(filename, child->name) < 0) {
          target_index = i;
        }
      }

      directory->children_count += 1;
      while (directory->children_count >= directory->children_cap) {
        directory->children_cap *= 2;
        directory->children = realloc(
          directory->children,
          directory->children_cap * sizeof(Node)
        );
      }

      for (int j = directory->children_count; j > target_index; j--) {
        directory->children[j] = directory->children[j - 1];
      }

      Node new_node;
      strcpy(new_node.name, filename);
      new_node.type = T_File;
      new_node.data.file = malloc(sizeof(FileData));

      new_node.data.file->contents = malloc(sizeof(char));
      new_node.data.file->contents[0] = '\0';
      new_node.data.file->contents_cap = 0;
      new_node.data.file->contents_len = 0;

      new_node.data.file->ref_count = 1;
      directory->children[target_index] = new_node;

      filename = strtok(NULL, " ");
    }
  } else if (command.type == ECHO_INTO) {
    Node *node = get_node_by_path(
      &state->root,
      make_absolute(state, command.data.echo.path)
    );

    FileData *file = node->data.file;

    size_t required_len = strlen(command.data.echo.contents);

    if (command.data.echo.overwrite) {
      required_len += file->contents_len;
    }

    if (required_len > file->contents_cap) {
      file->contents_len = required_len;
      while (file->contents_cap < required_len + 1) {
        file->contents_cap = 2 * file->contents_cap + 1;
        file->contents = realloc(
          file->contents,
          file->contents_cap * sizeof(char)
        );
      }
    }

    if (command.data.echo.overwrite) {
      strcpy(file->contents, command.data.echo.contents);
    } else {
      strcat(file->contents, command.data.echo.contents);
    }
  }

  return false;
}

int main(int argc, char *argv[]) {
  State state = {0};
  strcpy(state.cwd.path, "/");
  state.root.type = T_Directory;
  state.root.data.directory.children_count = 1;
  state.root.data.directory.children_cap = 1;

  Node *root = malloc(sizeof(Node));
  state.root.data.directory.children = root;
  strcpy(root->name, "");
  root->type = T_Directory;
  root->data.directory.children_count = 0;
  root->data.directory.children_cap = 1;
  root->data.directory.children = malloc(sizeof(Node));

  while (true) {
    Command command = parse_command(&state);
    if (exec_command(&state, command)) {
      return 0;
    }
  }
}
