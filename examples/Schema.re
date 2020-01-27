module ModelGenerator = ModelUtils.GenerateModel(Domain.RootModel);
module Seat : Domain.SCHEMA_TYPE = ModelGenerator();