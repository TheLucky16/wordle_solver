#!/usr/bin/env python3

import time
import random
from operator import itemgetter
from collections import Counter
from datetime import date, datetime
import numpy as np
import pandas as pd
import sys
import os
import chromedriver_autoinstaller

from selenium import webdriver
from selenium.webdriver.common.keys import Keys
from selenium.webdriver.chrome.service import Service as ChromeService
from selenium.webdriver.chrome.options import Options
from selenium.webdriver.common.by import By
from selenium.webdriver.common.action_chains import ActionChains
from selenium.common.exceptions import NoSuchElementException
from selenium.webdriver.support.ui import WebDriverWait
from selenium.webdriver.support import expected_conditions as EC 
from webdriver_manager.chrome import ChromeDriverManager

chromedriver_autoinstaller.install()
chrome_options = webdriver.ChromeOptions()
chrome_options.add_argument("--headless")
chrome_options.add_argument("--no-sandbox")
chrome_options.add_argument("--disable-dev-shm-usage")
chrome_options.add_argument("--window-size=1920,1080")
driver = webdriver.Chrome(options = chrome_options)

startingwords=['SLATE', 'CRANE', 'SLANT', 'TRACE', 'CARTE', 'CRATE']
nextword = random.choice(startingwords) #chooses random starting word from the list above
Wordlist = np.array('aback abase abate abbey abbot abhor abide abled abode abort about above abuse abyss acorn acrid actor acute adage adapt adept admin admit adobe adopt adore adorn adult affix afire afoot afoul after again agape agate agent agile aging aglow agony agree ahead aider aisle alarm album alert algae alibi alien align alike alive allay alley allot allow alloy aloft alone along aloof aloud alpha altar alter amass amaze amber amble amend amiss amity among ample amply amuse angel anger angle angry angst anime ankle annex annoy annul anode antic anvil aorta apart aphid aping apnea apple apply apron aptly arbor ardor arena argue arise armor aroma arose array arrow arson artsy ascot ashen aside askew assay asset atoll atone attic audio audit augur aunty avail avert avian avoid await awake award aware awash awful awoke axial axiom axion azure bacon badge badly bagel baggy baker baler balmy banal banjo barge baron basal basic basil basin basis baste batch bathe baton batty bawdy bayou beach beady beard beast beech beefy befit began begat beget begin begun being belch belie belle belly below bench beret berry berth beset betel bevel bezel bible bicep biddy bigot bilge billy binge bingo biome birch birth bison bitty black blade blame bland blank blare blast blaze bleak bleat bleed bleep blend bless blimp blind blink bliss blitz bloat block bloke blond blood bloom blown bluer bluff blunt blurb blurt blush board boast bobby boney bongo bonus booby boost booth booty booze boozy borax borne bosom bossy botch bough boule bound bowel boxer brace braid brain brake brand brash brass brave bravo brawl brawn bread break breed briar bribe brick bride brief brine bring brink briny brisk broad broil broke brood brook broom broth brown brunt brush brute buddy budge buggy bugle build built bulge bulky bully bunch bunny burly burnt burst bused bushy butch butte buxom buyer bylaw cabal cabby cabin cable cacao cache cacti caddy cadet cagey cairn camel cameo canal candy canny canoe canon caper caput carat cargo carol carry carve caste catch cater catty caulk cause cavil cease cedar cello chafe chaff chain chair chalk champ chant chaos chard charm chart chase chasm cheap cheat check cheek cheer chess chest chick chide chief child chili chill chime china chirp chock choir choke chord chore chose chuck chump chunk churn chute cider cigar cinch circa civic civil clack claim clamp clang clank clash clasp class clean clear cleat cleft clerk click cliff climb cling clink cloak clock clone close cloth cloud clout clove clown cluck clued clump clung coach coast cobra cocoa colon color comet comfy comic comma conch condo conic copse coral corer corny couch cough could count coupe court coven cover covet covey cower coyly crack craft cramp crane crank crash crass crate crave crawl craze crazy creak cream credo creed creek creep creme crepe crept cress crest crick cried crier crime crimp crisp croak crock crone crony crook cross croup crowd crown crude cruel crumb crump crush crust crypt cubic cumin curio curly curry curse curve curvy cutie cyber cycle cynic daddy daily dairy daisy dally dance dandy datum daunt dealt death debar debit debug debut decal decay decor decoy decry defer deign deity delay delta delve demon demur denim dense depot depth derby deter detox deuce devil diary dicey digit dilly dimly diner dingo dingy diode dirge dirty disco ditch ditto ditty diver dizzy dodge dodgy dogma doing dolly donor donut dopey doubt dough dowdy dowel downy dowry dozen draft drain drake drama drank drape drawl drawn dread dream dress dried drier drift drill drink drive droit droll drone drool droop dross drove drown druid drunk dryer dryly duchy dully dummy dumpy dunce dusky dusty dutch duvet dwarf dwell dwelt dying eager eagle early earth easel eaten eater ebony eclat edict edify eerie egret eight eject eking elate elbow elder elect elegy elfin elide elite elope elude email embed ember emcee empty enact endow enema enemy enjoy ennui ensue enter entry envoy epoch epoxy equal equip erase erect erode error erupt essay ester ether ethic ethos etude evade event every evict evoke exact exalt excel exert exile exist expel extol extra exult eying fable facet faint fairy faith false fancy fanny farce fatal fatty fault fauna favor feast fecal feign fella felon femme femur fence feral ferry fetal fetch fetid fetus fever fewer fiber ficus field fiend fiery fifth fifty fight filer filet filly filmy filth final finch finer first fishy fixer fizzy fjord flack flail flair flake flaky flame flank flare flash flask fleck fleet flesh flick flier fling flint flirt float flock flood floor flora floss flour flout flown fluff fluid fluke flume flung flunk flush flute flyer foamy focal focus foggy foist folio folly foray force forge forgo forte forth forty forum found foyer frail frame frank fraud freak freed freer fresh friar fried frill frisk fritz frock frond front frost froth frown froze fruit fudge fugue fully fungi funky funny furor furry fussy fuzzy gaffe gaily gamer gamma gamut gassy gaudy gauge gaunt gauze gavel gawky gayer gayly gazer gecko geeky geese genie genre ghost ghoul giant giddy gipsy girly girth given giver glade gland glare glass glaze gleam glean glide glint gloat globe gloom glory gloss glove glyph gnash gnome godly going golem golly gonad goner goody gooey goofy goose gorge gouge gourd grace grade graft grail grain grand grant grape graph grasp grass grate grave gravy graze great greed green greet grief grill grime grimy grind gripe groan groin groom grope gross group grout grove growl grown gruel gruff grunt guard guava guess guest guide guild guile guilt guise gulch gully gumbo gummy guppy gusto gusty gypsy habit hairy halve handy happy hardy harem harpy harry harsh haste hasty hatch hater haunt haute haven havoc hazel heady heard heart heath heave heavy hedge hefty heist helix hello hence heron hilly hinge hippo hippy hitch hoard hobby hoist holly homer honey honor horde horny horse hotel hotly hound house hovel hover howdy human humid humor humph humus hunch hunky hurry husky hussy hutch hydro hyena hymen hyper icily icing ideal idiom idiot idler idyll igloo iliac image imbue impel imply inane inbox incur index inept inert infer ingot inlay inlet inner input inter intro ionic irate irony islet issue itchy ivory jaunt jazzy jelly jerky jetty jewel jiffy joint joist joker jolly joust judge juice juicy jumbo jumpy junta junto juror kappa karma kayak kebab khaki kinky kiosk kitty knack knave knead kneed kneel knelt knife knock knoll known koala krill label labor laden ladle lager lance lanky lapel lapse large larva lasso latch later lathe latte laugh layer leach leafy leaky leant leapt learn lease leash least leave ledge leech leery lefty legal leggy lemon lemur leper level lever libel liege light liken lilac limbo limit linen liner lingo lipid lithe liver livid llama loamy loath lobby local locus lodge lofty logic login loopy loose lorry loser louse lousy lover lower lowly loyal lucid lucky lumen lumpy lunar lunch lunge lupus lurch lurid lusty lying lymph lyric macaw macho macro madam madly mafia magic magma maize major maker mambo mamma mammy manga mange mango mangy mania manic manly manor maple march marry marsh mason masse match matey mauve maxim maybe mayor mealy meant meaty mecca medal media medic melee melon mercy merge merit merry metal meter metro micro midge midst might milky mimic mince miner minim minor minty minus mirth miser missy mocha modal model modem mogul moist molar moldy money month moody moose moral moron morph mossy motel motif motor motto moult mound mount mourn mouse mouth mover movie mower mucky mucus muddy mulch mummy munch mural murky mushy music musky musty myrrh nadir naive nanny nasal nasty natal naval navel needy neigh nerdy nerve never newer newly nicer niche niece night ninja ninny ninth noble nobly noise noisy nomad noose north nosey notch novel nudge nurse nutty nylon nymph oaken obese occur ocean octal octet odder oddly offal offer often olden older olive ombre omega onion onset opera opine opium optic orbit order organ other otter ought ounce outdo outer outgo ovary ovate overt ovine ovoid owing owner oxide ozone paddy pagan paint paler palsy panel panic pansy papal paper parer parka parry parse party pasta paste pasty patch patio patsy patty pause payee payer peace peach pearl pecan pedal penal pence penne penny perch peril perky pesky pesto petal petty phase phone phony photo piano picky piece piety piggy pilot pinch piney pinky pinto piper pique pitch pithy pivot pixel pixie pizza place plaid plain plait plane plank plant plate plaza plead pleat plied plier pluck plumb plume plump plunk plush poesy point poise poker polar polka polyp pooch poppy porch poser posit posse pouch pound pouty power prank prawn preen press price prick pride pried prime primo print prior prism privy prize probe prone prong proof prose proud prove prowl proxy prude prune psalm pubic pudgy puffy pulpy pulse punch pupil puppy puree purer purge purse pushy putty pygmy quack quail quake qualm quark quart quash quasi queen queer quell query quest queue quick quiet quill quilt quirk quite quota quote quoth rabbi rabid racer radar radii radio rainy raise rajah rally ralph ramen ranch randy range rapid rarer raspy ratio ratty raven rayon razor reach react ready realm rearm rebar rebel rebus rebut recap recur recut reedy refer refit regal rehab reign relax relay relic remit renal renew repay repel reply rerun reset resin retch retro retry reuse revel revue rhino rhyme rider ridge rifle right rigid rigor rinse ripen riper risen riser risky rival river rivet roach roast robin robot rocky rodeo roger rogue roomy roost rotor rouge rough round rouse route rover rowdy rower royal ruddy ruder rugby ruler rumba rumor rupee rural rusty sadly safer saint salad sally salon salsa salty salve salvo sandy saner sappy sassy satin satyr sauce saucy sauna saute savor savoy savvy scald scale scalp scaly scamp scant scare scarf scary scene scent scion scoff scold scone scoop scope score scorn scour scout scowl scram scrap scree screw scrub scrum scuba sedan seedy segue seize semen sense sepia serif serum serve setup seven sever sewer shack shade shady shaft shake shaky shale shall shalt shame shank shape shard share shark sharp shave shawl shear sheen sheep sheer sheet sheik shelf shell shied shift shine shiny shire shirk shirt shoal shock shone shook shoot shore shorn short shout shove shown showy shrew shrub shrug shuck shunt shush shyly siege sieve sight sigma silky silly since sinew singe siren sissy sixth sixty skate skier skiff skill skimp skirt skulk skull skunk slack slain slang slant slash slate sleek sleep sleet slept slice slick slide slime slimy sling slink sloop slope slosh sloth slump slung slunk slurp slush slyly smack small smart smash smear smell smelt smile smirk smite smith smock smoke smoky smote snack snail snake snaky snare snarl sneak sneer snide sniff snipe snoop snore snort snout snowy snuck snuff soapy sober soggy solar solid solve sonar sonic sooth sooty sorry sound south sower space spade spank spare spark spasm spawn speak spear speck speed spell spelt spend spent sperm spice spicy spied spiel spike spiky spill spilt spine spiny spire spite splat split spoil spoke spoof spook spool spoon spore sport spout spray spree sprig spunk spurn spurt squad squat squib stack staff stage staid stain stair stake stale stalk stall stamp stand stank stare stark start stash state stave stead steak steal steam steed steel steep steer stein stern stick stiff still stilt sting stink stint stock stoic stoke stole stomp stone stony stood stool stoop store stork storm story stout stove strap straw stray strip strut stuck study stuff stump stung stunk stunt style suave sugar suing suite sulky sully sumac sunny super surer surge surly sushi swami swamp swarm swash swath swear sweat sweep sweet swell swept swift swill swine swing swirl swish swoon swoop sword swore sworn swung synod syrup tabby table taboo tacit tacky taffy taint taken taker tally talon tamer tango tangy taper tapir tardy tarot taste tasty tatty taunt tawny teach teary tease teddy teeth tempo tenet tenor tense tenth tepee tepid terra terse testy thank theft their theme there these theta thick thief thigh thing think third thong thorn those three threw throb throw thrum thumb thump thyme tiara tibia tidal tiger tight tilde timer timid tipsy titan tithe title toast today toddy token tonal tonga tonic tooth topaz topic torch torso torus total totem touch tough towel tower toxic toxin trace track tract trade trail train trait tramp trash trawl tread treat trend triad trial tribe trice trick tried tripe trite troll troop trope trout trove truce truck truer truly trump trunk truss trust truth tryst tubal tuber tulip tulle tumor tunic turbo tutor twang tweak tweed tweet twice twine twirl twist twixt tying udder ulcer ultra umbra uncle uncut under undid undue unfed unfit unify union unite unity unlit unmet unset untie until unwed unzip upper upset urban urine usage usher using usual usurp utile utter vague valet valid valor value valve vapid vapor vault vaunt vegan venom venue verge verse verso verve vicar video vigil vigor villa vinyl viola viper viral virus visit visor vista vital vivid vixen vocal vodka vogue voice voila vomit voter vouch vowel vying wacky wafer wager wagon waist waive waltz warty waste watch water waver waxen weary weave wedge weedy weigh weird welch welsh whack whale wharf wheat wheel whelp where which whiff while whine whiny whirl whisk white whole whoop whose widen wider widow width wield wight willy wimpy wince winch windy wiser wispy witch witty woken woman women woody wooer wooly woozy wordy world worry worse worst worth would wound woven wrack wrath wreak wreck wrest wring wrist write wrong wrote wrung wryly yacht yearn yeast yield young youth zebra zesty zonal'.split(" ")) 
absents = '' 
notx = ['', '', '', '', '']
repeats = []
theword = [0,0,0,0,0]
game_outcome = 0.5







def Filter(absents, notx, repeats, theword):
    possibles = []
    notx_string = ''.join(notx)
    
    for word in Wordlist:
        if not set(absents).intersection(word) and all(char in word for char in notx_string):
            if all(word[idx] != char for idx, chars in enumerate(notx) for char in chars):
                if all(theword[idx] == 0 or theword[idx] == word[idx] for idx in range(5)):
                    if all(word.count(char) == count for char, count in repeats):
                        possibles.append(word)
                        
    return np.array(possibles)

def Letter_Density(possibles):
    possibles_array = np.array([list(word) for word in possibles])
    nextWord = np.zeros((5, len(possibles_array)), dtype='<U1')
    
    for idx in range(5):
        nextWord[idx] = possibles_array[:, idx]
    
    letter_counts = [Counter(nextWord[idx]) for idx in range(5)]
    
    sorted_density = [sorted(count.items(), key=itemgetter(1), reverse=True) for count in letter_counts]
    
    return sorted_density

csv_solves_path = 'Wordle_Solves.csv'
date_path = 'Last_Saved_Date.txt'

def append_to_csv(data):
    solves_df = pd.DataFrame([data],columns=["word","attempts","date","attempt_1","attempt_2","attempt_3","attempt_4","attempt_5","attempt_6"])
    solves_df.to_csv(csv_solves_path,mode='a', header=not os.path.isfile(csv_solves_path), index=False)
    with open(csv_solves_path, 'a') as f:
        f.write(','.join(map(str, data)) + '\n')

def is_data_already_saved_today():
    try:
        with open(date_path, 'r') as f:
            last_saved_date_str = f.readline().strip()
            if last_saved_date_str:
                last_saved_date = datetime.strptime(last_saved_date_str, '%d.%m.%Y').date()
                return last_saved_date == date.today()
            else:
                return False
    except FileNotFoundError:
        return False
    
def save_last_saved_date():
    with open(date_path, 'w') as f:
        f.write(date.today().strftime('%d.%m.%Y'))

# open website:
wait = WebDriverWait(driver,5)
driver.get("https://www.nytimes.com/games/wordle/index.html")
time.sleep(10)

try:
    element = wait.until(EC.element_to_be_clickable((By.XPATH, '//*[@id="fides-button-group"]/div[1]/button[1]')))
    element.click() # reject cookies
except Exception:
    pass # what if there are no cookies


driver.find_element(By.XPATH,"/html/body/div/div/div/div/div/div[2]/button[2]").click() #  play

try:
    element = wait.until(EC.element_to_be_clickable((By.XPATH, "/html/body/div/div/dialog/div/div/button")))
    element.click() #how to play
except Exception as e:
    print("An error occurred:", e)
    driver.quit()
time.sleep(0.5)
action = ActionChains(driver)

# Scroll down


#if sys.platform == 'darwin':
    #cmd_ctrl = Keys.COMMAND 
#else:
    #Keys.CONTROL #windows/mac compatability
action.key_down(Keys.COMMAND).send_keys(Keys.ARROW_DOWN).perform()
action.key_up(Keys.COMMAND).perform()




for Repeat in range(1,7):
    
    #input word
    
    time.sleep(1)
    action.send_keys(nextword).perform()
    action.send_keys(Keys.RETURN).perform()
    time.sleep(1.5)
    
    #Read all Rows:

    Rows = driver.find_elements(By.CLASS_NAME,"Row-module_row__pwpBq")
    Words = []
    for row in Rows:
        word = []
        Letters = row.find_elements(By.CLASS_NAME ,'Tile-module_tile__UWEHN')
        for letter in Letters:
            letter_aria = letter.get_attribute("aria-label").split(' ')
            letter_aria.pop(1)
            if len(letter_aria) == 6:
                for l in range(3):
                    letter_aria.pop(-1)
            letter_aria[0] = int(letter_aria[0][0]) - 1
            if letter_aria[1]!='empty':
                letter_aria[1] = letter_aria[1][0].lower()
            word.append(letter_aria)
        Words.append(word)


    # Append filtering data: 
    
    prescor = '' #for repeats (presents and corrects)
    
    for row in range(5,-1,-1):
        if Words[row]!= [[0, 'empty'], [1, 'empty'], [2, 'empty'], [3, 'empty'], [4, 'empty']]:
            current_row = Words[row]
            last_row_index = row 
            break
    current_word = ''
    for idx in range(5):
        item = current_row[idx]
        char = item[1] #character
        status = item[-1]
        if status == 'correct':
            theword[item[0]] = char
            prescor += char
        elif status == 'present':
            notx[item[0]] += char
            prescor+=char
        elif status == 'absent':
            absents += char
        
        current_word += char
    
    if all(item[2] == 'correct' for item in current_row):
        game_outcome = 1
        break 
    elif Repeat == 6 and any(item[2] != 'correct' for item in current_row):
        game_outcome = 0
        
    for idx in range(len(prescor)):
        char = current_word[idx]
        if prescor.count(char) == 2 and [char,2] not in repeats:
            repeats.append([char,2])
        elif prescor.count(char) == 3 and [char,3] not in repeats:
            repeats.append([char,3]) # there are no words with 4 or 5 repeating letters
    
    for idx in range(5):
        char = current_word[idx]
        if current_word.count(char)>1 and current_row[idx][2] != 'absent':
            newabsents = ''
            for abs_char in absents:
                if abs_char != char:
                    newabsents+=abs_char
            absents = newabsents
        if prescor.count(char) == 1 and current_word.count(char) > 1 and [char,1] not in repeats:
            repeats.append([char,1])
        
            

    # Filter and Sort the possible words

    possibles = Filter(absents, notx, repeats, theword)
    topLetters = Letter_Density(possibles)

    def scorer(word, topLetters):
        score = 0 
        for idx, char in enumerate(word):
            for letter, freq in topLetters[idx]:
                if char == letter:
                    score += freq
        return (word, score)


    ranked = sorted([scorer(word, topLetters) for word in possibles], key=itemgetter(1), reverse=True)
    nextword = ranked[0][0]

    
result = []
for item in Words:
    if item[0][1] == "empty":
        result.append('')
    else:
        row_result = item[0][2][0]+item[1][2][0]+item[2][2][0]+item[3][2][0]+item[4][2][0]
        result.append(row_result)    
current_date = date.today().strftime("%d.%m.%Y")
solved_data = [nextword,last_row_index + 1, current_date]+result

if not is_data_already_saved_today() and game_outcome != 0:
    append_to_csv(solved_data)
    save_last_saved_date()
    print("game won in", len(result), "tries")
elif is_data_already_saved_today():
    print("Data has already been saved today.")
elif game_outcome == 0:
    print("yer a feckin loosa")

time.sleep(4)
driver.quit()

