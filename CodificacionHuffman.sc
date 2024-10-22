enum ArbolHuffman:
  @Override
  case HojaHuff(caracter: Char, peso:Int)
  case RamaHuff(nodoIzq : ArbolHuffman, nodoDch: ArbolHuffman)

  // Método para devolver el peso
  def peso: Int =
    @annotation.tailrec
    def pesoAux(arbol:ArbolHuffman) : Int = this match
      case HojaHuff(_, p) => p
      case RamaHuff(nodoIzq, nodoDch) => pesoAux(nodoIzq) + pesoAux(nodoDch)
    pesoAux(this)

  // Método para devolver la lista de caracteres
  def caracteres: List[Char] =
    @annotation.tailrec
    def caracteresAux(arbol: ArbolHuffman): List[Char] = this match
      case HojaHuff(caracter, _) => List(caracter)
      case RamaHuff(nodoIzq, nodoDch) => caracteresAux(nodoIzq) ::: caracteresAux(nodoDch)
    caracteresAux(this)



  // Construcción del árbol de Huffman
  val hojaS = ArbolHuffman.HojaHuff('S', 4)
  val hojaO = ArbolHuffman.HojaHuff('O', 3)
  val hojaE = ArbolHuffman.HojaHuff('E', 2)
  val hojaEspacio = ArbolHuffman.HojaHuff(' ', 2)

  // Las ramas intermedias
  val ramaEO = ArbolHuffman.RamaHuff(hojaE, hojaEspacio)
  val ramaFinal = ArbolHuffman.RamaHuff(hojaO, ramaEO)

  //Arbol final
  val arbolHuffman = ArbolHuffman.RamaHuff(hojaS, ramaFinal)

  //Comprobamos los metodos
  println(arbolHuffman.peso)
  println(arbolHuffman.caracteres)
end ArbolHuffman
