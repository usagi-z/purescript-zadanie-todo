// data Node :: Type

// persistTodos :: Array TodoItem -> Effect Unit
exports.persistTodos = function (todos) {
    localStorage.setItem('todos',JSON.stringify(todos));
};

// loadTodos :: Effect (Array TodoItem)
exports.loadTodos = function () {
    let t = JSON.parse(localStorage.getItem('todos'));
    if (t == null) {
        return [];
    } else {
        return t;
    }
}

// window :: Effect Node
exports.window = function () {
  return window;
};
