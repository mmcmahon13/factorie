package cc.factorie.app.nlp.load

import cc.factorie.app.nlp.{Token, Sentence, Document}

import scala.collection.mutable.ArrayBuffer

/**
  * Created by Molly on 2/12/2016.
  */
object LoadRcvWiki {

  // load docs assuming that a line constitutes one document, as in rcv-wiki-text
  def fromCorpusFilename(file: String): Seq[Document] = {
    val documents = new ArrayBuffer[Document]
    for (line <- io.Source.fromFile(file).getLines()) {
      // create a new document
      var document = new Document(line)
      // get a list of the doc's sentences
      // for each of them, create a sentence associate with teh document object, fill it with tokens
      val sentences = line.split("[.?!;]")
      for(sentence <- sentences){
        val docSentence = new Sentence(document)
        val words = sentence.split(' ')
        for(word <- words){
          val t = new Token(docSentence, word)
        }
      }
      // add the document to the list and return the final sequence of docs
      documents += document
    }
    documents
  }

  // load a single doc from a filename
  def fromDocFilename(file: String): Document = {
    val document = new Document()
    for (line <- io.Source.fromFile(file).getLines()) {
      val sentences = line.split("[.?!;]")
      for(sentence <- sentences){
        val docSentence = new Sentence(document)
        val words = sentence.split(' ')
        for(word <- words){
          val t = new Token(docSentence, word)
        }
      }
    }
    document
  }
}

