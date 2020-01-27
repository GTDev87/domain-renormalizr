module ModelGenerator = ModelUtils.GenerateModel(Domain.RootModel);

module SchemaSeat : Domain.SCHEMA_TYPE = ModelGenerator();

module M = ModelUtils.BuildModel(Seat_Record, SchemaSeat, FragmentUtils.CreateFakeFragment(Seat_Record));