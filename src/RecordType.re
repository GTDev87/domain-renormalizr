module Type = {
  type t('model, 'local) = {
    data: 'model,
    local: 'local,
  };
  
  type _data;
}
