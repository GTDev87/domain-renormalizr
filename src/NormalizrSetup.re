let tryAgainifNullOption = (optionA, optionB) =>
  switch (optionA) {
  | Some(_) => optionA
  | None => optionB
  };

let removeOptionsFromList = (list : list(option('a))) : list('a) =>
  Belt.List.reduce(list, [], (memo, ele) =>
    switch(ele){
    | None => memo
    | Some(value) => memo @ [value]
    }
  );

module type NORMALIZR_STORE = {
  type normalizedType = NormalizrNew.normalizedSchema(Domain.RootModel.t, Type.uuid, Domain.RootModel.record);
  
  let getNormalized: unit => normalizedType;
  let updateNormalized: (normalizedType) => Js.Promise.t(normalizedType);
};

module type SOURCE_CONTAINER = {
  type normalizedType;
  type idType;
  type domainType;
  let getRecord: (normalizedType, idType) => option(domainType);
};

module type RESOURCE_REDUCER = {
  type normalizedType;
  type idType;
  type domainType;
  type domainAction;
  type defaultParam;
  let getRecord: (normalizedType, idType) => option(domainType);

  let getRecordWithDefault: (normalizedType, idType, defaultParam) => domainType;
  let updateWithDefault: (defaultParam) => (Js.Promise.t(normalizedType), idType, domainAction) => Js.Promise.t(normalizedType);
  let createUpdateIdWithDefault: (idType, defaultParam, normalizedType) => (domainAction) => Js.Promise.t(normalizedType);
};

module DomainTypeConverter = (
  NormalizrGenerator : ModelUtils.NORMALIZR_GENERATOR_TYPE
    with type normalizedType = NormalizrNew.normalizedSchema(Domain.RootModel.t, Type.uuid, Domain.RootModel.record),
  DomainType: Domain.M
) => {
  module Container = DomainType.Container;
  module Wrapper = DomainType.Record.Wrapper;

  let normalizerGetItemFromSchema: (NormalizrGenerator.normalizedType, DomainType.Model.idType) => option(Domain.RootModel.record) =
    (
      a =>
        a
        |> DomainType.Model.idToRootId
        |> NormalizrGenerator.modelIdToIdFunction
    )
    |> NormalizrNew.Normalizr.getItemFromSchema;

  let idListToFilteredItems = (idList: list(DomainType.Model.idType), modelTypefunction) =>
    idList
    |> Belt.List.map(_, modelTypefunction)
    |> removeOptionsFromList;

  let update = (
    default: DomainType.Model.idType => DomainType.Model.Record.t,
    getterFn:
      (NormalizrGenerator.normalizedType, DomainType.Model.idType) =>
      option(DomainType.Model.Record.t),
    reduce:
      (DomainType.Action.action, Js.Promise.t(DomainType.Action.model)) =>
      Js.Promise.t(DomainType.Action.model),
    normalized: Js.Promise.t(NormalizrGenerator.normalizedType),
    id: DomainType.Model.idType,
    action: DomainType.Action.action,
  ) : Js.Promise.t(NormalizrGenerator.normalizedType) => {
    normalized
    |> Js.Promise.then_(norm =>
        norm
        |> getterFn(_, id)
        |> Belt.Option.getWithDefault(_, default(id))
        |> Js.Promise.resolve)
    |> reduce(action, _)
    |> (modelPromise) => (normalized, modelPromise)
    |> Js.Promise.all2
    |> Js.Promise.then_(((norm, model)) =>
        Js.Promise.resolve(NormalizrGenerator.normalizerCommitItemToSchema(norm, Wrapper.wrap(model))));
  };
  let fromLocal = (
    normalized: NormalizrGenerator.normalizedType,
    id: DomainType.Model.idType /* maybe make domain model idtype*/,
  ) : option(DomainType.Model.Record.t) =>
    normalized
    |> normalizerGetItemFromSchema(_, id)
    |> Belt.Option.flatMap(_, Wrapper.unwrap);

  let fromSchema = (
    normalized: NormalizrGenerator.normalizedType,
    id: DomainType.Model.idType /* maybe make domain model idtype*/,
  ) : option(DomainType.Model.Record.t) => {
    let optionNormalized = fromLocal(normalized, id);

    Wrapper.apolloEnabled ?
      id
      |> Container.getById
      |> Belt.Option.map(_, (fragment: DomainType.Model.Fragment.Fields.t) => {
           (
             switch (optionNormalized) {
             | Some(data) => {
                 {
                    local: data.local,
                    data: DomainType.Model.Record.Data.fromObject(fragment),
                  }
               }
             | None => DomainType.Model.Record.fromObject(fragment) /* does this also generate the local? */
             }: DomainType.Model.Record.t
           )
        })
      |> tryAgainifNullOption(_, optionNormalized) :
      optionNormalized;
  };

  module GenerateConverterFunc = (
    SourceContainer : 
      SOURCE_CONTAINER
        with type normalizedType = NormalizrGenerator.normalizedType
        and type idType = DomainType.Model.idType
        and type domainType = DomainType.Model.Record.t
  ) : (
    RESOURCE_REDUCER
      with type normalizedType = NormalizrGenerator.normalizedType
      and type idType = DomainType.Model.idType
      and type domainType = DomainType.Model.Record.t
      and type defaultParam = DomainType.Model.Record.defaultParam
      and type domainAction = DomainType.Action.action
  ) => {
    type normalizedType = NormalizrGenerator.normalizedType;
    type idType = DomainType.Model.idType;
    type domainType = DomainType.Model.Record.t;
    type defaultParam = DomainType.Model.Record.defaultParam;
    type domainAction = DomainType.Action.action;

    let getRecord = SourceContainer.getRecord;
    let getRecordWithDefault = (
      normalized: NormalizrGenerator.normalizedType,
      id: DomainType.Model.idType,
      param: DomainType.Model.Record.defaultParam,
    ) : DomainType.Model.Record.t =>
      normalized
      |> SourceContainer.getRecord(_, id)
      |> Belt.Option.getWithDefault(_, DomainType.Model.Record.defaultWithId(param, id));
  
    let updateWithDefault = (param: defaultParam) =>
      update(
        DomainType.Model.Record.defaultWithId(param, _),
        getRecord, /* How does this handle the llocal??????? */
        DomainType.Action.reduce);
  
    let createUpdateIdWithDefault = (
      id: idType,
      param: defaultParam,
      normalized: normalizedType,
    ) => (action) => updateWithDefault(param, normalized |> Js.Promise.resolve, id, action);
  };

  module LocalSourceContainer = {
    type normalizedType = NormalizrGenerator.normalizedType;
    type idType = DomainType.Model.idType;
    type domainType = DomainType.Model.Record.t;
    let getRecord = fromLocal;
  };

  module RemoteSourceContainer = {
    type normalizedType = NormalizrGenerator.normalizedType;
    type idType = DomainType.Model.idType;
    type domainType = DomainType.Model.Record.t;
    let getRecord = fromSchema;
  };

  module Local = GenerateConverterFunc(LocalSourceContainer);
  module Remote = GenerateConverterFunc(RemoteSourceContainer);

  module WithStore = (
    NormalizeStore : NORMALIZR_STORE
      with type normalizedType = NormalizrGenerator.normalizedType
  ) => {
    module AddStoreFunctions = (ResourceReducer : RESOURCE_REDUCER) => {
      let getRecord = (id) => Local.getRecord(NormalizeStore.getNormalized(), id);
  
      let getRecordWithDefault = (
        id: DomainType.Model.idType,
        param: DomainType.Model.Record.defaultParam,
      ) : DomainType.Model.Record.t =>
        Local.getRecordWithDefault(NormalizeStore.getNormalized(), id, param);
    
      let updateWithDefault = (param: DomainType.Model.Record.defaultParam) =>
        Local.updateWithDefault(param, NormalizeStore.getNormalized() |> Js.Promise.resolve);
    
      let createUpdateIdWithDefault = (
        id: DomainType.Model.idType,
        param: DomainType.Model.Record.defaultParam,
      ) => Local.createUpdateIdWithDefault(id, param, NormalizeStore.getNormalized());
    };

    module Local = AddStoreFunctions(Local);
    module Remote = AddStoreFunctions(Remote);
  }
};