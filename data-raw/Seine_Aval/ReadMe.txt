
Bonjour Clara,

Voici toute l'information dont tu as besoin pour brancher le cas de la Seine aval.

- Dossier Bathymetry:

Information sur toutes les sections transversales de l'ensemble du projet.

- Dossier Gaugings: 

RData avec les jaugeages à Aizier [[1]], à Heurteauville [[2]] et à Rouen [[3]]. Toutes les mesures sont en UTC+1


- Dossier Stage_Raw_Data: 
Données brutes des limnigrammes disponibles (Attention, faisceau horaire UTC+1). 
Néanmoins, j'ai fait un trie de l'information dont tu as besoin pour que tu ne perds pas
de temps là-dessus. Je te propose de lire directement le fichier input_h_Q_Qmec_Seine.RData avec l'info du débit aussi intégrée

-Fichier input_h_Q_Qmec_Seine.RData:
Résumé de l'information nécessaire pour le cas de la Seine aval. 

obs_t est le nom du .RData chargé. Il s'agit d'un data frame avec une grille du temps discretisé depuis 2015-09-25 01:00:00 
jusqu'au 2015-10-31 23:00:00 à un pas de temps de 5 minutes. 

Ensuite, les hauteurs d'eau (h_...) mesurées sur la totalités des stations qui ont des mesures sur la période défini précédemment.
Certaines valeurs NA apparaissent à cause de troux dans séries temporelles. Il faut remarquer et écrire dans le rapport 
que les hauteurs sont mesurées dans le faisceau horaire UTC+1.

Enfin, les derniers colonnes avec les entête avec le motif de "Q_..." correspondent aux mesures du débit
avec leurs incertitudes (uQ_...) exprimées comme une fois l'écart-type. En ce qui concerne le faisceau horaire, elle ont été déjà prétraité pour les avoir en UTC+1.
La date du jaugeage a été approximée à l'heure discretisée la plus proche possible. 

- Fichier estuarie-Seine.jepg : photo de l'influence de la marée (jusqu'au la barrage de Poses).

- Chart datum information.txt :

Information par rapport aux CD de toutes les mesures associées.


Afin de pouvoir comparer éventuellement les différentes approaches (Qmec et Modèle hydrodynamique 1D), 
je te propose de utiliser les mêmes données que j'avais utilisé pour l'étude.

Date initiale de la simulation : 2015-09-28 01:10:00
Campagne de jaugeages : 2015-09-29 00:00:00

A priori Ks -> point de départ 37, distribution = 'LogNormal', paramètres de la distribution (3.7,0.1)
