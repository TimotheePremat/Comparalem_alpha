####Remove all lemma ending on 'é', because they regularly have non-schwa final vowels written "é" before a consonant (-ez, -et, etc.)
NOM_E_cleaned <- NOM_E %>%
  filter(!str_detect(lemma, ".*é"))
NOM_nonE_cleaned <- NOM_nonE %>%
  filter(!str_detect(lemma, ".*é"))
####Remove all lemma ending on 'eur', because they regularly are imparisyllabic. Note that all lemmas in this case or not removed, as some have a lemma written "or" ("seignor"), "eur"/"ëor" ("empereur"/"emperëor"), etc.
NOM_E_cleaned <- NOM_E_cleaned %>%
		filter(!str_detect(lemma, ".*(eur|ëor|eor|eör)"))
NOM_nonE_cleaned <- NOM_nonE_cleaned %>%
		filter(!str_detect(lemma, ".*(eur|ëor|eor|eör)"))

####Remove imparisyllabic terms
NOM_E_cleaned <- NOM_E_cleaned %>%
  filter(lemma!='seignor') %>%
		filter(lemma!='homme') %>%
		filter(lemma!='ome') %>%
		filter(lemma!='prudhomme') %>%
		filter(lemma!='empereur') %>%
		filter(lemma!='emperëor') %>%
		filter(lemma!='comte') %>%
		filter(lemma!='conte') %>%
		filter(lemma!='neveu') %>%
		filter(lemma!='traïtor') %>%
		filter(lemma!='larron') %>%
		filter(lemma!='felon') %>%
		filter(lemma!='none') %>%
		filter(lemma!='jugeur') %>%
		filter(lemma!='maire') %>%
		filter(lemma!='poigneur') %>%
		filter(lemma!='prêcheur') %>%
		filter(lemma!="pejor") %>%
		filter(lemma!="traître") %>%
		filter(lemma!="enfant") %>%
		filter(lemma!="träitor") %>%
		filter(lemma!="martyr") %>%
		filter(lemma!="ancessor")
NOM_nonE_cleaned <- NOM_nonE_cleaned %>%
  filter(lemma!='seignor') %>%
		filter(lemma!='homme') %>%
		filter(lemma!='ome') %>%
		filter(lemma!='prudhomme') %>%
		filter(lemma!='empereur') %>%
		filter(lemma!='emperëor') %>%
		filter(lemma!='comte') %>%
		filter(lemma!='conte') %>%
		filter(lemma!='neveu') %>%
		filter(lemma!='traïtor') %>%
		filter(lemma!='larron') %>%
		filter(lemma!='felon') %>%
		filter(lemma!='none') %>%
		filter(lemma!='jugeur') %>%
		filter(lemma!='maire') %>%
		filter(lemma!='poigneur')%>%
		filter(lemma!='prêcheur') %>%
		filter(lemma!="pejor") %>%
		filter(lemma!="traître") %>%
		filter(lemma!="enfant") %>%
		filter(lemma!="träitor") %>%
		filter(lemma!="martyr") %>%
		filter(lemma!="ancessor")
####Remove gender inflected terms
NOM_E_cleaned <- NOM_E_cleaned %>%
  filter(lemma!='ami') %>%
		filter(lemma!='bonnier') %>%
		filter(lemma!='fol') %>%
		filter(lemma!='châtelain') %>%
		filter(lemma!='bourgeois') %>%
		filter(lemma!='voisin')  %>%
		filter(lemma!='fou') %>%
		filter(lemma!='cosin') %>%
		filter(lemma!='contrait') %>%
		filter(lemma!='dru') %>%
		filter(lemma!='peschëor') %>%
		filter(lemma!='prisonier') %>%
		filter(lemma!='galois') %>%
		filter(lemma!='colombe') %>%
		filter(lemma!='meschin') %>%
		filter(lemma!='prest') %>%
		filter(lemma!='rëont') %>%
		filter(lemma!='tortiz') %>%
		filter(lemma!='voir') %>%
		filter(lemma!="beguin") %>%
		filter(lemma!="borjois")
NOM_nonE_cleaned <- NOM_nonE_cleaned %>%
  filter(lemma!='ami') %>%
		filter(lemma!='bonnier') %>%
		filter(lemma!='fol') %>%
		filter(lemma!='châtelain') %>%
		filter(lemma!='bourgeois')  %>%
		filter(lemma!='voisin')  %>%
		filter(lemma!='fou')  %>%
		filter(lemma!='cosin') %>%
		filter(lemma!='contrait') %>%
		filter(lemma!='dru') %>%
		filter(lemma!='parçonnier') %>%
		filter(lemma!='peschëor') %>%
		filter(lemma!='prisonier') %>%
		filter(lemma!='galois') %>%
		filter(lemma!='colombe') %>%
		filter(lemma!='meschin') %>%
		filter(lemma!='prest') %>%
		filter(lemma!='rëont') %>%
		filter(lemma!='tortiz') %>%
		filter(lemma!='voir')

####Remove non-schwa final <e>
NOM_E_cleaned <- NOM_E_cleaned %>%
  filter(lemma!='nef') %>%
		filter(lemma!='tref') %>%
		filter(lemma!='forest') %>%
		filter(lemma!='ciel') %>%
		filter(lemma!='enemi') %>%
		filter(lemma!='ennemi') %>%
		filter(lemma!='paix') %>%
		filter(lemma!='pied') %>%
		filter(lemma!='plaid') %>%
		filter(lemma!='fait') %>%
		filter(lemma!='clef') %>%
		filter(lemma!='rechef') %>%
		filter(lemma!='œil') %>%
		filter(lemma!='rai') %>%
		filter(lemma!='ues') %>%
		filter(lemma!='forfait')  %>%
		filter(lemma!='pais') %>%
		filter(lemma!='fauteuil') %>%
		filter(lemma!='lait') %>%
		filter(lemma!='parçonnier') %>%
		filter(lemma!='plait') %>%
		filter(lemma!='agait') %>%
		filter(lemma!='aguet') %>%
		filter(lemma!='ais') %>%
		filter(lemma!='lez') %>%
		filter(lemma!='algier') %>%
		filter(lemma!='entrait') %>%
		filter(lemma!='trait') %>%
		filter(lemma!='valet') %>%
		filter(lemma!='brait') %>%
		filter(lemma!='confès') %>%
		filter(lemma!='espiet') %>%
		filter(lemma!='mes') %>%
		filter(lemma!='vœu') %>%
		filter(lemma!='fief') %>%
		filter(lemma!='nasel') %>%
		filter(lemma!='amiral') %>%
		filter(lemma!="atrait") %>%
		filter(lemma!="berz") %>%
		filter(lemma!="bienfait") %>%
		filter(lemma!="brief")  %>%
		filter(lemma!="buef")  %>%
		filter(lemma!="chief1")
NOM_nonE_cleaned <- NOM_nonE_cleaned %>%
		filter(lemma!='nef') %>%
		filter(lemma!='tref') %>%
		filter(lemma!='forest') %>%
		filter(lemma!='ciel') %>%
		filter(lemma!='enemi') %>%
		filter(lemma!='ennemi') %>%
		filter(lemma!='paix') %>%
		filter(lemma!='pied') %>%
		filter(lemma!='plaid') %>%
		filter(lemma!='fait') %>%
		filter(lemma!='clef') %>%
		filter(lemma!='rechef') %>%
		filter(lemma!='œil') %>%
		filter(lemma!='rai') %>%
		filter(lemma!='ues') %>%
		filter(lemma!='forfait')  %>%
		filter(lemma!='pais') %>%
		filter(lemma!='fauteuil') %>%
		filter(lemma!='lait') %>%
		filter(lemma!='plait') %>%
		filter(lemma!='agait') %>%
		filter(lemma!='aguet') %>%
		filter(lemma!='ais') %>%
		filter(lemma!='lez') %>%
		filter(lemma!='algier') %>%
		filter(lemma!='entrait') %>%
		filter(lemma!='trait') %>%
		filter(lemma!='valet') %>%
		filter(lemma!='brait') %>%
		filter(lemma!='confès') %>%
		filter(lemma!='espiet') %>%
		filter(lemma!='mes') %>%
		filter(lemma!='vœu') %>%
		filter(lemma!='fief') %>%
		filter(lemma!='nasel') %>%
		filter(lemma!='amiral') %>%
		filter(lemma!="atrait")

####Remove abreviated forms
NOM_E_cleaned <- NOM_E_cleaned %>%
  filter(lemma!='livre')
NOM_nonE_cleaned <- NOM_nonE_cleaned %>%
		filter(lemma!='livre')

####Remove lemmatization errors
NOM_E_cleaned <- NOM_E_cleaned %>%
  filter(lemma!='pro') %>% #One "prodomes" lemmatized as "pro" (CharettesKu)
		filter(lemma!='corir') %>% #Two forms "cort" are tagged as NOM while they are VERcjg (CharettesKu, CligesKu)
		filter(lemma!='faire') %>% #One form "face" tagged lemma="faire" while it is clearly the word for "face" (visage)
		filter(lemma!='vain') %>% #One form "vainne" is tagged lemma="vain" while it is clearly the word for "veine/blood vessel" (ErecKu)
		filter(lemma!='ouvrière') %>% #One form "ouvriers" is tagged lemma="ouvrière", while it is about male workers (SGenPr1).
		filter(lemma!='voiz') %>% #One form "voiz" is tagged lemma="voie" while it is the voice and note VIA (roland).
		filter(lemma!='vaisseau') %>%
		filter(lemma!="äunee")  %>%
		filter(lemma!="coche2-coque") %>%
		filter(lemma!="coup2")
NOM_nonE_cleaned <- NOM_nonE_cleaned %>%
		filter(lemma!='pro') %>%
		filter(lemma!='corir') %>%
		filter(lemma!='faire') %>%
		filter(lemma!='vain') %>%
		filter(lemma!='ouvrière') %>%
		filter(lemma!='voiz') %>%
		filter(lemma!="äunee") %>%
		filter(lemma!="boivre")

####Remove POS errors
NOM_E_cleaned <- NOM_E_cleaned %>%
  filter(lemma!='bon') %>% #ADJqua 'boenes' tagged type:ADJqua but fropos:NOM (ErecKu)
		filter(lemma!='jëuner') %>% #One forme "jeüné" (with lemma="jëuner") is tagged NOM while it is a participium passivum (PercevalKu).
  filter(lemma!='partir') %>% #One form "partie" is tagged NOM lemma="partir" while it is the participium passivum (ErecKu).
		filter(lemma!="oster")  %>%
		filter(lemma!="cesser")  %>%
		filter(lemma!="chëoir")
NOM_nonE_cleaned <- NOM_nonE_cleaned %>%
		filter(lemma!='bon') %>%
	 filter(lemma!='jëuner') %>%
		filter(lemma!='jëuner') %>%
		filter(lemma!='vaisseau') %>%
		filter(lemma!="oster")

####Remove other stuff (consonantal cluster, etc.)
NOM_E_cleaned <- NOM_E_cleaned %>%
  filter(lemma!='tempest') %>%
		filter(lemma!="boiot-boët")
NOM_nonE_cleaned <- NOM_nonE_cleaned %>%
  filter(lemma!='tempest')
