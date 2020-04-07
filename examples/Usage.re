
module FullReduced = ModelUtils.AddRecord(Seat.Record, ModelUtils.EmptyNormalizr(Domain.RootModel));

module SeatConverter = NormalizrSetup.DomainTypeConverter(FullReduced, Seat);

let normalized = [];

let promiseNormalized = Js.Promise.resolve(normalized);

let seatId = Seat.Model.idToTypedId("");

let optionSeat = SeatConverter.Remote.getRecord(normalized, seatId);

let seat = SeatConverter.Remote.getRecordWithDefault(normalized, seatId);

let updateSeat = SeatConverter.Remote.updateWithDefault((), promiseNormalized, seatId, Seat_Action.None);

let updateSeat2 = SeatConverter.Remote.createUpdateIdWithDefault(seatId, (), normalized);

module NormalizrStore = {
  type normalizedType = FullReduced.normalizedType;
  let getNormalized = () => [];
  let updateNormalized = (normalizr) => Js.Promise.resolve(normalizr);
};

module SeatWithNormalizr = SeatConverter.WithStore(NormalizrStore);

let optionSeat = SeatWithNormalizr.Remote.getRecord(seatId);
let seat = SeatWithNormalizr.Remote.getRecordWithDefault(seatId);
let updateSeat = SeatWithNormalizr.Remote.updateWithDefault((), seatId, Seat_Action.None);
let updateSeat2 = SeatWithNormalizr.Remote.createUpdateIdWithDefault(seatId, ());
