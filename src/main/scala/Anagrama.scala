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
   }yield (n, palabra.count((x) => x==n))).toList

    ocurrenciasPal.distinct
  }
  /**
   * Recibe una frase, que es una lista de palabras, vuelve esas palabras listas de caracteres, luego las empaqueta y
   * cuenta cuantas veces se repite cada letra de la frase.
   * @param frase -> lista[palabras]
   * @return ocurrencias -> lista( lista(caracter, veces que se repite))
   */
  def lOcFra(frase: Frase):Ocurrencias = {
    lOcPal(frase.flatten)
  }

  /**
   * Evaluacion peresoza para crear un hashMap de listas de palabras ascociadas pr su mismo tipo de ocurrencia.
   */
  lazy val diccionarioPorOcurrencias:Map[Ocurrencias, List[Palabra]] = {
    diccionario.groupBy((pal:Palabra)=>lOcPal(pal).sorted)
  }

  def anagramasDePalabras(palabra: Palabra): List[Palabra] = {

    def anagramAux(ocurrencias: Ocurrencias): List[Palabra] = {
    val ocurrenciaPal = (for {
      n <- diccionarioPorOcurrencias
      if (n._1==ocurrencias)
    } yield n._2)
      ocurrenciaPal.flatten
    }.toList

    anagramAux(lOcPal(palabra))

  }

}
