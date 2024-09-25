defmodule Dispatcher do
  use Matcher
  define_accept_types [
    html: [ "text/html", "application/xhtml+html" ],
    json: [ "application/json", "application/vnd.api+json" ]
  ]

  @any %{}
  @json %{ accept: %{ json: true } }
  @html %{ accept: %{ html: true } }

  define_layers [ :static, :services, :fall_back, :not_found ]

  # In order to forward the 'themes' resource to the
  # resource service, use the following forward rule:
  #
  # match "/themes/*path", @json do
  #   Proxy.forward conn, path, "http://resource/themes/"
  # end
  #
  # Run `docker-compose restart dispatcher` after updating
  # this file.

  ###############
  # STATIC
  ###############

  # self-service
  match "/index.html", %{layer: :static} do
    forward(conn, [], "http://frontend/index.html")
  end

  get "/assets/*path", %{layer: :static} do
    forward(conn, path, "http://frontend/assets/")
  end

  get "/@appuniversum/*path", %{layer: :static} do
    forward(conn, path, "http://frontend/@appuniversum/")
  end

  #################
  # FRONTEND PAGES
  #################

  # self-service
  match "/*path", %{layer: :fall_back, accept: %{html: true}} do
    # we don't forward the path, because the app should take care of this in the browser.
    forward(conn, [], "http://frontend/index.html")
  end


  ################
  # RESOURCES
  ################

  match "/resource/*path", @json do
    Proxy.forward( conn, path, "http://resource/" )
  end

  match "/aanduidingsobjects/*path", @json do
    Proxy.forward( conn, path, "http://resource/aanduidingsobjects/" )
  end

  match "/locations/*path", @json do
    Proxy.forward( conn, path, "http://resource/locations/" )
  end

  match "/addresses/*path", @json do
    Proxy.forward( conn, path, "http://resource/addresses/" )
  end

  match "/besluits/*path", @json do
    Proxy.forward( conn, path, "http://resource/besluits/" )
  end

  match "/concepts/*path", @json do
    Proxy.forward( conn, path, "http://resource/concepts/" )
  end

  match "/annotations/*path", @json do
    Proxy.forward( conn, path, "http://resource/annotations/" )
  end

  match "/annotation-feedbacks/*path", @json do
    Proxy.forward( conn, path, "http://resource/annotation-feedbacks/" )
  end

  match "/annotation-types/*path", @json do
    Proxy.forward( conn, path, "http://resource/annotation-types/" )
  end

  match "/agents/*path", @json do
    Proxy.forward( conn, path, "http://resource/agents/" )
  end

  match "/*_", %{ layer: :not_found } do
    send_resp( conn, 404, "Route not found.  See config/dispatcher.ex" )
  end
end
