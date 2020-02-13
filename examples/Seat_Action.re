/* done */
type action =
  | None;

type model = Seat_Model.M.Record.t;

let rec reduce = (action, promise: Js.Promise.t(model)): Js.Promise.t(model) =>
  promise
  |> Js.Promise.then_((card: model) => {
       switch (action) {
       /* both below */
       | None => card |> Js.Promise.resolve
       };
     });