module CreateFakeFragment(RecordType : Domain.RECORD) : (
  Domain.FRAGMENT
    with type data = RecordType._data
) {
  type data = RecordType._data;

  module Fields = {
    type t = {id: string};
    let name: string = "";
    let query: string = "";
    let parse = (_): t => {id: ""};
  };
  
  let fragmentType = "";

  let toId = (_) => "";
  
  let fromObject = (obj: Fields.t): data => RecordType._defaultData(RecordType.defaultParamData, toId(obj));
}