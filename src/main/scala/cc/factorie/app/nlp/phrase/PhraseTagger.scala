package cc.factorie.app.nlp.phrase

/**
  * Created by Molly on 1/21/2016.
  */

import cc.factorie.app.nlp.lemma.LowercaseLemmatizer
import cc.factorie.app.nlp.load.LoadRcvWiki
import cc.factorie.app.nlp.{SharedNLPCmdOptions, DocumentAnnotator, Token, Document}
import cc.factorie.app.nlp.lexicon.TriePhraseLexicon
import cc.factorie.app.strings.{nonWhitespaceClassesSegmenter, StringSegmenter}
import cc.factorie.variable.{CategoricalDomain, CategoricalVariable}

import scala.io.Source

// pipeline version of a phrase tagger annotator
object PhraseTagger extends DocumentAnnotator{

  // lexicon to hold phrases
  object PhraseLexicon extends TriePhraseLexicon("Phrases", nonWhitespaceClassesSegmenter, LowercaseLemmatizer)

  // by default, load lexicon from the serialized "phrases" file
  def process(document:Document): Document = {
    tagText(document, "phrases.txt")
    return document
  }

  // read in phrases from lexicon file and build PhraseLexicon
  def buildLexicon(lexFile: String) = {
    println("Building the lexicon")
    for (line <- Source.fromFile(lexFile, "ISO-8859-1").getLines) {
      //println(line)
      PhraseLexicon += line
    }
    println()
  }

  // for each sentence in the document, tag the tokens with a  PhraseType (CategoricalVariable[String])
  def tagText(text:Document, lexFile: String) = {
    buildLexicon(lexFile)
    for (sentence <- text.sentences)
      PhraseLexicon.labelLemmatizedText(sentence.tokens, (t:Token, s:String)=>new PhraseTypeTag(t,s+"-Phrase"))
  }

  /** A categorical variable, associated with a token, holding whether and where it occurs in a phrase. */
  class PhraseTypeTag(token:Token, initialIndex:Int)
    extends CategoricalVariable[String](initialIndex) {
    def this(token:Token, initialCategory:String) = this(token, PhraseTagDomain.index(initialCategory))
    final def domain = PhraseTagDomain
    def isBeginningPhrase = PhraseTagDomain.isBeginningPhrase(categoryValue)
    def isInPhrase = PhraseTagDomain.isInPhrase(categoryValue)
    def isEndPhrase = PhraseTagDomain.isEndPhrase(categoryValue)
    def isNotPhrase = PhraseTagDomain.isNotPhrase(categoryValue)
    def isUnitPhrase = PhraseTagDomain.isUnitPhrase(categoryValue)
  }

  object PhraseTagDomain extends CategoricalDomain[String] {
    this ++= Vector(
      "B-Phrase",
      "I-Phrase",
      "L-Phrase",
      "O-Phrase",
      "U-Phrase"
    )

    def isBeginningPhrase(tag:String): Boolean = {tag=="B-Phrase"}
    def isInPhrase(tag:String): Boolean = {tag=="I-Phrase"}
    def isEndPhrase(tag:String): Boolean = {tag=="L-Phrase"}
    def isNotPhrase(tag:String): Boolean = {tag=="O-Phrase"}
    def isUnitPhrase(tag:String): Boolean = {tag=="U-Phrase"}

  }

  // TODO: do we need prerequisite attrs?
  override def prereqAttrs: Iterable[Class[_]] = Nil
  // tokens tagged with PhraseTypeTags as attributes
  override def postAttrs: Iterable[Class[_]] = List(classOf[PhraseTypeTag])
  // TODO: get these from tokens attr or something?
  override def tokenAnnotationString(token: Token): String = ???

  //TODO: add more options
  class PhraseTaggerOptions extends cc.factorie.util.DefaultCmdOptions with SharedNLPCmdOptions {
    val docFile = new CmdOption("doc-file", "", "FILENAME", "Document to annotate")
    val corpFile = new CmdOption("corp-file", "", "FILENAME", "File of multiple documents to annotate")
    //val docFiles = new CmdOption("doc-files", "", "String", " comma-separated list of documents to annotate")
    val lexiconFile = new CmdOption("lexicon-file", "phrases.txt", "FILENAME", "Serialized lexicon to load")
  }

  def main(args: Array[String]): Unit = {
    val opts = new PhraseTaggerOptions
    opts.parse(args)

    if(opts.corpFile.wasInvoked){
      println("tagging document "+opts.corpFile.value)
      val documents = LoadRcvWiki.fromCorpusFilename(opts.corpFile.value)
      for(document <- documents) {
        val d = process(document)
        var name = ""
        for (token <- d.tokens) {
          if(token.attr[PhraseTypeTag] != null){
            name = "["+token.attr[PhraseTypeTag].toString()+"]"
          } else {
            token.attr += new PhraseTypeTag(token,"O-Phrase")
            name = "["+token.attr[PhraseTypeTag].toString()+"]"
          }
          println(token.string + " " + name)
        }
      }
    }
  }


}

