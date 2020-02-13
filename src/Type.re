type uuid = string;

module type CLIENT = {
  let instance: ReasonApolloReadFragment.Type.generatedApolloClient;
};