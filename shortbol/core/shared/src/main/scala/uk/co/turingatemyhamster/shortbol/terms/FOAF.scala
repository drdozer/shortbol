package uk.co.turingatemyhamster.shortbol
package terms

import shorthandAst.{sugar, NSPrefix}

import sugar._

/**
  * Created by nmrp3 on 23/11/16.
  */
object FOAF {
  val foaf: NSPrefix = "foaf"

  object Core {
    val Agent = foaf :# "Agent"
    val Person = foaf :# "Person"
    val name = foaf :# "name"
    val title = foaf :# "title"
    val img = foaf :# "img"
    val depiction = foaf :# "depiction"
    val familyName = foaf :# "familyName"
    val givenName = foaf :# "givenName"
    val knows = foaf :# "knows"
    val based_near = foaf :# "based_near"
    val age = foaf :# "age"
    val made = foaf :# "made"
    val primaryTopic = foaf :# "primaryTopic"
    val Project = foaf :# "Project"
    val Organization = foaf :# "Organization"
    val Group = foaf :# "Group"
    val member = foaf :# "Member"
    val Document = foaf :# "Document"
    val Image = foaf :# "Image"
  }

  object SocialWeb {
    val nick = foaf :# "nic"
    val mbox = foaf :# "mbox"
    val homepage = foaf :# "homepage"
    val weblog = foaf :# "weblog"
    val openid = foaf :# "openid"
    val jabberID = foaf :# "jabberID"
    val mbox_sha1sum = foaf :# "mbox_sha1sum"
    val interest = foaf :# "interest"
    val topic_interest = foaf :# "topic_interest"
    val topic = foaf :# "topic"
    val workplaceHomepage = foaf :# "workplaceHomepage"
    val workInfoHomepage = foaf :# "workInfoHomepage"
    val schoolHomepage = foaf :# "schoolHomepage"
    val publications = foaf :# "publications"
    val currentProject = foaf :# "currentProject"
    val pastProject = foaf :# "pastProject"
    val account = foaf :# "account"
    val accountServiceHomepage = foaf :# "accountServiceHomepage"
    val PersonalProfileDocument = foaf :# "PersonalProfileDocument"
    val tipjar = foaf :# "tipjar"
    val sha1 = foaf :# "sha1"
    val thumbnail = foaf :# "thumbnail"
    val logo = foaf :# "logo"
  }
}
