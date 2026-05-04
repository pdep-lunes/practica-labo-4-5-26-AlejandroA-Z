data Perro=Perritos{
  raza::String,
  juguetes::[String],
  tiempoGuarderia::Int,
  energia::Int
}deriving(Show,Eq)

zara=Perritos{
  raza=dalmata,
  juguetes=[pelota,mantita],
  tiempoGuarderia= 90,              --90 minutos
  energia=80
}deriving(Show,Eq)



jugar:: Perro->Perro                       
jugar mascota = modificarEnergia ((-)10) mascota 


ladrar::Perro->Int->Perro                 
ladrar mascota ladridos = modificarEnergia (div ladridos) mascota


modificarEnergia::(Int->Int)->Perro->Perro   
modificarEnergia cambioenergia mascota = mascota { energia = max (cambioenergia (energia mascota) 0) }


regalar:: String->Perro->Perro
regalar regalo mascota = mascota{juguetes= modificarjuguetes mascota regalo}


modificarjuguetes::Perro->String->[String]
modificarjuguetes mascota regalo | regalos==“resta” = tail (juguetes mascota)
                                 | otherwise = regalo ++ juguetes mascota


diaDeSpa::Perro->Perro
diaDeSpa mascota  | tiempoGuarderia mascota >=50 || verificarRaza mascota = (modificarEnergia (const 100) . regalar "Peine de goma") mascota
                  | otherwise= mascota


verificarRaza::Perro->Bool
verificarRaza mascota | raza mascota == “dalmata”
                      | raza mascota == “pomerania”
                      | otherwise = False  --Por defecto otherwise es TRUE


diaDeCampo::Perro->Perro
diaDeCampo mascota = mascota{energia= energia (jugar mascota), juguetes= modificarjuguetes mascota “resta” }

                             


type Actividad = (Perro -> Perro, Int)

data Guarderia= UnaGuarderia{
 actividades :: [Actividad]
}deriving(Show,Eq)

pdeperritos = UnaGuarderia {
  actividades=[(jugar,30), (ladrar,20), (regalarPelota,0),(diaSpa, 120),(diaCampo,720)]
}

--tiempo de duracion de la rutina 890 minutos

estarGuarderia::Perro->Bool
estarGuarderia mascota | tiempoGuarderia mascota > 890
                       | otherwise

perroResponsable::Perro->Bool
perroResponsable mascota = filter (3<).length  (juguetes (diaDeCampo mascota)) 


