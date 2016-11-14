{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Security where
---------------------------------Roles------------------------------------------
roleClient="cliente"
roleRestaurnt="restaurante"
roleUser="usuario"
-------------------------------URL servicios------------------------------------
serviceBienvenida="/"
serviceObternerTodosLosMenus="/menus" --esto pa que?
serviceObtenerMenuPorId="/menus/id"
serviceAgregarMenu="/menu"
serviceRegistrarse="/clientes"
serviceObternerTodosLosClientes="/clientes" --Esto pa que?
serviceIniciarSesion="/iniciarSesion"
serviceCerrarSesion="/cerrarSesion"
-------------------------------Autorizacion-------------------------------------
getAutoriy=
  [(serviceBienvenida,roleUser),
  (serviceObternerTodosLosMenus,roleRestaurnt),
  (serviceObtenerMenuPorId,roleRestaurnt)]

postAutority=
  [(serviceAgregarMenu,roleRestaurnt),
  (serviceRegistrarse,roleUser)]

putAutority=
  [(serviceIniciarSesion,roleUser),
  (serviceCerrarSesion,roleClient),
  (serviceCerrarSesion,roleRestaurnt)]
