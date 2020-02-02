type _data = {
  id: string,
  seatNumber: int,
};


module Local = ModelUtils.CreateFakeLocal();
type _record = RecordType.Type.t(_data, Local.Record.t);

type defaultParam = unit;
let defaultParamData: defaultParam = ();

let _defaultData = (_default, id) => {
  {
    id,
    seatNumber: 0,
  }
};

let _defaultRecordId = (defaultPar, id): _record => {
  data: _defaultData(defaultPar, id),
  /* local: Classroom_Local.Model.Record.default(id), */
  local: Local.Record.default(id),
};

let _defaultRecord = (): _record => {
  _defaultRecordId(defaultParamData, "id")
};


let _defaultWithId = ((): defaultParam, id: Type.uuid) => _defaultRecordId(defaultParamData, id);

let findId = (record : _record) => record.data.id;
