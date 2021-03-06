type uuid = string;

module type CONTAINER_INTERFACE = {
  module Fragment: ReasonApolloReadFragment.ReadFragment.M;
};

module type LOCAL_RECORD = {
  type _record;
  
  let _defaultRecord: (Type.uuid) => _record;
  
  module Record: {
    type t = _record;
    let default: (Type.uuid) => _record;
  };
};

module type ROOT_MODEL = {
  type t = ..;
  type id = ..;
  type record = ..;
  type data;

  type t += EMPTY_T;
  type id += EMPTY_ID;
  type record += EMPTY_RECORD;
};

module RootModel : ROOT_MODEL = {
  type t = ..;
  type id = ..;
  type record = ..;
  type data;

  /* This is here until https://github.com/facebook/reason/issues/1597 */
  /* https://github.com/ocaml/ocaml/pull/1546 */
  type t += EMPTY_T;
  type id += EMPTY_ID;
  type record += EMPTY_RECORD;
};

module SchemaType = {
  type t = Type;
};

module type RECORD = {
  module Local: LOCAL_RECORD;

  type _data;
  type _record = RecordType.Type.t(_data, Local.Record.t);

  type defaultParam;

  let defaultParamData: defaultParam;
  
  let _defaultData: (defaultParam, Type.uuid) => _data;
  let _defaultRecordId: (defaultParam, Type.uuid) => _record;
  let _defaultRecord: (defaultParam) => _record;
  let findId: (_record) => Type.uuid;

  let _defaultWithId: (defaultParam, Type.uuid) => _record;
};

type typeWithId = {. "id": string};

module type SCHEMA_TYPE = {

  module Root: ROOT_MODEL;

  /* module CreatedType: SchemaType; */

  type RootModel.id += Id(Type.uuid);
  type RootModel.t += Schema;

  type id;
  type _t =  SchemaType.t;

  let idToString: (id) => Type.uuid
  let idToRootId: (id) => RootModel.id;
  let stringToId: (Type.uuid) => id;
};

module type MODEL = {
  module ModelSchemaType : SCHEMA_TYPE;
  module InternalSchema : ROOT_MODEL;

  type _local;

  module ModelRecord : RECORD;
  type _data = ModelRecord._data;
  type _record = ModelRecord._record;

  type idType = ModelSchemaType.id;
  type rootIdType = RootModel.id;

  let idToRootId: (ModelSchemaType.id) => RootModel.id;
  let getUUIDFromId: (ModelSchemaType.id) => Type.uuid;
  let idToTypedId: (Type.uuid) => idType;

  module rec Fragment: {
    module Fields: ReasonApolloTypes.Config;
    let toId: Fields.t => Type.uuid;
  }
  and Record: {
    type t = _record;
    type defaultParam;

    type defaultFn = (defaultParam, ModelSchemaType.id) => t;
    let findId: (ModelRecord._record) => Type.uuid;
    let default: defaultParam => ModelRecord._record;

    let defaultParamData: defaultParam;

    // module Local: LOCAL_RECORD;

    module Data: {
      type t = _data;
      let fromObject: Fragment.Fields.t => t;
    };
    let fromObject: Fragment.Fields.t => t;
    let defaultWithId: defaultFn;
  };

  let objectToId: (Fragment.Fields.t) => idType;

  let fragmentType: string;
  let fragmentName: string;
  let _defaultData: (Record.defaultParam, Type.uuid) => _data;
};

module type DOMAIN_WRAPPER = {
  type model;
  type rootRecord = RootModel.record;
  let wrap: model => rootRecord;
  let unwrap: rootRecord => option(model);
  let apolloEnabled: bool;
};

module type MODEL_RECORD {
  module Model: MODEL;
  module Wrapper: DOMAIN_WRAPPER;

  type model;
  type _data;
  type _record;

  type RootModel.record += Record(Model.Record.t);

};

module type FRAGMENT = {
  type data;

  module Fields: {
    type t;
    let name: string;
    let query: string;
    let parse: ('a) => t;
  };
  
  let fragmentType: string;
  
  let fromObject: (Fields.t) => data;
  let toId: (Fields.t) => Type.uuid;
};

module type CONTAINER {
  type idType;
  type config;
  type record;

  let getById: idType => option(config);
  let getRecordById: idType => option(record);

  [@bs.obj] external makeProps:
      (~id: 'id, ~children: 'children, ~key: string=?, unit) => {. "children": 'children, "id": 'id} = "";

  let make: ({ . "children": (record => React.element), "id": string}) => React.element;
};

module type ACTION = {
  type action;
  type model;
  let reduce: (action, Js.Promise.t(model)) => Js.Promise.t(model);
};

module type M = {
  module rec Model: MODEL
  and Action: (
    ACTION
      with type model = Model.Record.t
  )
  and Container: (
    CONTAINER 
      with type idType = Model.idType
      and type record = Model.Record.t
      and type config = Model.Fragment.Fields.t
  )
  and Record: (
    MODEL_RECORD
      with type Wrapper.model = Action.model
  )
};
