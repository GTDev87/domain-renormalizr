module Model = Seat_Model.M;

module ModelSchema = Schema.Seat;
module Record = ModelUtils.AddModel(ModelSchema, Model);

module Action = Seat_Action;
module Container = ApolloFragment.GenerateContainer(Client, ReasonApolloReadFragment.ReadFragment.M)(Model);
