object Anagrama {

  type Palabra = String
  type Frase = List[Palabra]
  type Ocurrencias = List[(Char, Int)]
  val diccionario : List[Palabra] = List( "cosas" , "como" , "yo" , "y" , "ocasos" , "cayo" , "mocosos" , "roca" , "moco" , "sos")


  /**
   * Recibe una palabra como string, lo convierte a lista de carcateres para luego aplicar empaquetar y así contar
   * cuantas veces se repite cada letra en una palabra
   * @param palabra
   * @return ocurrencias -> lista( lista(caracter, veces que se repite))
   */
  def lOcPal(palabra: Palabra):Ocurrencias = {
    val ocurrenciasPal = (for {
      n <- palabra
   }yield (n, palabra.count(x => x==n))).toList

    ocurrenciasPal.distinct
  }
  /**
   * Recibe una frase, que es una lista de palabras, vuelve esas palabras listas de caracteres, luego las empaqueta y
   * cuenta cuantas veces se repite cada letra de la frase.
   * @param frase -> lista[palabras]
   * @return ocurrencias -> lista( lista(caracter, veces que se repite))
   */
  def lOcFra(frase: Frase):Ocurrencias = {
    val concatFrase = frase reduceLeft ((x,y)=>x+y)
    lOcPal(concatFrase).filter(_._1 !=' ')
  }

  /**
   * Evaluacion peresoza para crear un hashMap de listas de palabras ascociadas pr su mismo tipo de ocurrencia.
   */
  lazy val diccionarioPorOcurrencias:Map[Ocurrencias, List[Palabra]] = {
    diccionario.groupBy((pal:Palabra)=>lOcPal(pal).sorted)
  }


  /**
   * Funcin que recibe una palabra y busca en el diccionario las palabras que tienes sus mismas ocurrencias
   * @param palabra
   * @return
   */
  def anagramasDePalabras(palabra: Palabra): List[Palabra] = {

    def anagramAux(ocurrencias: Ocurrencias): List[Palabra] = {
    val ocurrenciaPal = (for {
      n <- diccionarioPorOcurrencias
      if n._1==ocurrencias
    } yield n._2)
      ocurrenciaPal.flatten
    }.toList

    anagramAux(lOcPal(palabra).sorted)

  }

  /**
   * Funcion que devuuelve todas las posibles combinaciones de una lista de ocurrencias
   * @param lOcurrencias
   * @return
   */
  def combinaciones(lOcurrencias: Ocurrencias):List[Ocurrencias]= lOcurrencias match {
    case Nil => List(List())
    case y::ys =>{
      val combinacion = combinaciones(ys)
      combinacion++
        (for {
          n<- combinacion
          i<- 1 to y._2
        }yield (y._1,i)::n)
    }
  }

  /**
   * Funcion que recibe una lista de ocurrencias y un subconjunto de esa lista, y devuelve el complemento
   * de este conjunto para completar la lista.
   * @param lOcurrencias
   * @param slOcurrencias
   * @return slOcurrencias'
   */
  def complemento(lOc: Ocurrencias, slOc: Ocurrencias): Ocurrencias ={
    val lista1 = lOc.sorted
    val lista2 = slOc.sorted

    if(lista2.isEmpty) lista1
    else{
      if(lista2.head._1 == lista1.head._1){
        if(lista2.head._2==lista1.head._2) complemento(lista1.tail,lista2.tail)
        else (lista2.head._1,lista1.head._2-lista2.head._2)::complemento(lista1.tail,lista2.tail)
      }else lista1.head::complemento(lista1.tail,lista2)
    }
  }

  /**
   * Funcion final que sirve para que dada una frase, que viene siendo una lista de palabras, devuelva
   * todas las posibles frase que se puedan hacer con esta, usando solamente las palabras del diccionario.
   * @param frase
   * @return List[frase]
   */
  def anagramaFrase(frase: Frase): List[Frase] = {
    def anaFraAux(ocurrencias: Ocurrencias): List[Frase] = {
      if (ocurrencias.isEmpty) List(Nil)
      else for {
        combinacion <- combinaciones(ocurrencias)
        palabra <- diccionarioPorOcurrencias getOrElse(combinacion, Nil)
        frase <- anaFraAux(complemento(ocurrencias, lOcPal(palabra)))
        if !combinacion.isEmpty
      }yield palabra::frase
    }
    anaFraAux(lOcFra(frase))
  }


}
