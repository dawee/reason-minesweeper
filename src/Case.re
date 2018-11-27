type t =
  | Empty
  | Bomb
  | BombsAround(int);

let component = ReasonReact.statelessComponent("Case");

let make = (~case: t, _children) => {
  ...component,
  render: _self => {
    let content =
      switch (case) {
      | Empty => ReasonReact.null
      | Bomb => ReasonReact.string("BA")
      | BombsAround(length) =>
        length
        |> string_of_int
        |> ReasonReact.string
      };

    <div className="case">(content)</div>
  }
};