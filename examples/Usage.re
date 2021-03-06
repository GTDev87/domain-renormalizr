
module FullReduced = ModelUtils.AddRecord(Seat.Record, ModelUtils.EmptyNormalizr(Domain.RootModel));

module SeatConverter = NormalizrSetup.DomainTypeConverter(FullReduced, Seat);

let normalized = [];

let promiseNormalized = Js.Promise.resolve(normalized);

let seatId = Seat.Model.idToTypedId("");

let optionSeat = SeatConverter.Remote.getRecord(normalized, seatId);

let seat = SeatConverter.Remote.getRecordWithDefault(normalized, seatId);

let updateSeat = SeatConverter.Remote.reduceWithDefault((), promiseNormalized, seatId, Seat_Action.None);

let updateSeat2 = SeatConverter.Remote.createReduceIdWithDefault(normalized, seatId, ());
let updateSeat3 = SeatConverter.Remote.createPromiseReduceIdWithDefault(promiseNormalized, seatId, ());

module NormalizrStore = {
  type normalizedType = FullReduced.normalizedType;
  let getNormalized = () => [];
  let getUpdateNormalized = () => (normalizr: Js.Promise.t(normalizedType)) => Js.Promise.resolve(());
};

module SeatWithNormalizr = SeatConverter.WithStore(NormalizrStore);

let optionSeat = SeatWithNormalizr.Remote.getRecord(seatId);
let seat = SeatWithNormalizr.Remote.getRecordWithDefault(seatId, ());
let updateSeat = SeatWithNormalizr.Remote.updateWithDefault((), seatId, Seat_Action.None);
let updateSeat2 = SeatWithNormalizr.Remote.createUpdateIdWithDefault(seatId, ());
let updateSeat2 = SeatWithNormalizr.Remote.createReduceIdWithDefault(seatId, ());

let updateSeat3 = SeatConverter.Source.Remote.updateWithPromiseDefault(
  NormalizrStore.getNormalized() |> Js.Promise.resolve,
  NormalizrStore.getUpdateNormalized(),
  (),
  seatId,
  Seat_Action.None
);

let seatCreatePromiseUpdateIdWithDefault = SeatConverter.Source.Remote.createPromiseUpdateIdWithDefault(
  NormalizrStore.getNormalized() |> Js.Promise.resolve,
  NormalizrStore.getUpdateNormalized(),
  seatId,
  (),
);

let seatCreatePromiseReduceIdWithDefault = SeatConverter.Source.Remote.createPromiseReduceIdWithDefault(
  NormalizrStore.getNormalized() |> Js.Promise.resolve,
  seatId,
  (),
);

let seatCreatePromiseUpdateIdWithDefault = SeatConverter.Source.Remote.createUpdateIdWithDefault(
  NormalizrStore.getNormalized(),
  NormalizrStore.getUpdateNormalized(),
  seatId,
  (),
);

let seatCreatePromiseReduceIdWithDefault = SeatConverter.Source.Remote.createReduceIdWithDefault(
  NormalizrStore.getNormalized(),
  seatId,
  (),
);


let optionSeat = SeatConverter.Source.Remote.getRecord(NormalizrStore.getNormalized(), seatId);