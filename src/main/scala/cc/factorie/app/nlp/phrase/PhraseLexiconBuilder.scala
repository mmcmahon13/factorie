package cc.factorie.app.nlp.phrase

import java.io.{FileOutputStream, PrintWriter}
import cc.factorie.app.nlp.SharedNLPCmdOptions
import cc.factorie.app.nlp.load.LoadRcvWiki
import cc.factorie.app.nlp.segment.BigramStatistics

/**
  * Created by Molly on 2/8/2016.
  */
object PhraseLexiconBuilder {

  // add all phrases with scores above the chosen threshold to the lexicon file
  def buildLexicon(corpus: String, lexFile: String, delta:Double, threshold:Double) = {
    val corpusDocs = LoadRcvWiki.fromCorpusFilename(corpus)
    val bigrams = new BigramStatistics
    bigrams.process(corpusDocs)
    val phrases = bigrams.getLikelyBigramPhrases(delta, threshold)
    val pw = new PrintWriter(new FileOutputStream(lexFile), false)
    for (phraseSequence <- phrases) {
      if(!phraseSequence(0).replaceAll("[\",;'/.!#$&%]", "").equals("") && !phraseSequence(1).replaceAll("[\",;'/.!#$&%]", "").equals("")) {
        println(phraseSequence)
        pw.write(phraseSequence(0) + " " + phraseSequence(1) + "\n")
      }
    }
    pw.close()
  }

  // todo: add options for directory of docs, list of docs?
  class PhraseLexiconOptions extends cc.factorie.util.DefaultCmdOptions with SharedNLPCmdOptions {
    val corpusFile = new CmdOption("corpus-file", "", "FILENAME", "Corpus file containing documents.")
    val lexiconFile = new CmdOption("lexicon-file", "phrases.txt", "FILENAME", "Lexicon file to write phrases to")
    val delta = new CmdOption("delta", 0.1, "DOUBLE", "parameter for phrase scoring")
    val threshold = new CmdOption("threshold", 0.6, "DOUBLE", "minimum score for phrase to be included in the lexicon")
  }

  def main(args: Array[String]): Unit = {
    val opts = new PhraseLexiconOptions
    opts.parse(args)
    assert(opts.corpusFile.wasInvoked)
    buildLexicon(opts.corpusFile.value, opts.lexiconFile.value, opts.delta.value, opts.threshold.value)
  }

}
