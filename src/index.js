import './main.css';
import { Elm } from './Main.elm';
import * as serviceWorker from './serviceWorker';
import imgPath from './test.png';

//Need to pass image path as a flag for Html.programWithFlags
const app = Elm.Main.init({
  node: document.getElementById('root'),
  flags: { src: imgPath }
});

app.ports.sendStuff.subscribe(data => {
    let dataToSend = {};
    let sentence = JSON.stringify(data);
    console.log("recieved sentence which is =" + sentence);
    let justLetters = trimInput(sentence);
    let joinedWord = scrambleAndReturn(sentence);
    scrambleAndSend(sentence);
    /*
    let indexedMap = createIndexedMap(justLetters);
    let spacesList = getSpaceIndexList(justLetters);
    let first;
    let second;
    let third;
    let fourth;
    let fifth;
    let wordIndexObj = breakUpIndexedMap(indexedMap, spacesList);

    switch (spacesList.length) {
      case 1 :
        console.log('Whoops');
      case 2 :
        first = wordIndexObj[0];
        second = wordIndexObj[1];
        third = wordIndexObj[2];

        dataToSend.first = first;
        dataToSend.second = second;
        dataToSend.third = third;
      case 3 :
        first = wordIndexObj[0]
        second = wordIndexObj[1]
        third = wordIndexObj[2]
        fourth = wordIndexObj[3]
        
        dataToSend.first = first;
        dataToSend.second = second;
        dataToSend.third = third;
        dataToSend.fourth = fourth;
      case 4 :
        first = wordIndexObj[0]
        second = wordIndexObj[1]
        third = wordIndexObj[2]
        fourth = wordIndexObj[3]
        fifth = wordIndexObj[4]

        dataToSend.first = first;
        dataToSend.second = second;
        dataToSend.third = third;
        dataToSend.fourth = fourth;
        dataToSend.fifth = fifth;
    }

    dataToSend.numberOfWords = spacesList.length + 1;
    dataToSend.joinedWord = joinedWord;
    //dataToSend.wordIndexObj = wordIndexObj;

    console.log('dTS=' + JSON.stringify(dataToSend));

    app.ports.receiveStuff.send({ value: dataToSend });
  */
});

function scrambleAndReturn(sentence) {
    let words = sentence.split(" ");
    let kept = [];
    console.log('words=' + words);
    words.forEach( word => {
      let final = evalThenScramble(word);
      kept.push(final);
    });
    //kept is an array
    let joinedWord = kept.join(" ");
    return joinedWord;
/*
    console.log(app.ports);
    app.ports.receiveStuff.send({ value: joinedWord });
*/
}

function scrambleAndSend(sentence) {
  let words = sentence.split(" ");
  let kept = [];
  console.log('words=' + words);
  words.forEach( word => {
    let final = evalThenScramble(word);
    kept.push(final);
  });
  //kept is an array
  let joinedWord = kept.join(" ");

  console.log('joinedWord=' + joinedWord);

  console.log(app.ports);
  app.ports.receiveScrambled.send({ value: joinedWord });

}
  
function evalThenScramble(w) {
    let word = w;
    if (word.length <= 3) {
      return word;
    } else {
      console.log('scrambling');
      let scrambled = scramble(word);
      return scrambled;
    }
  }

function scramble(w) {
  let word = w;
  let wordArray = word.split("");
  let scrambledWord = "";
  let firstLetter = wordArray.shift();
  let lastLetter = wordArray.pop();
  let middleLetterArray = wordArray;
  console.log('fl=' + firstLetter + 'll=' + lastLetter + 'mla=' + middleLetterArray)
  while (middleLetterArray.length > 0) {
    scrambledWord +=  middleLetterArray.splice(middleLetterArray.length * Math.random() << 0, 1);
  }
  console.log('sw=' + scrambledWord);
  let finalWord = firstLetter + scrambledWord + lastLetter;
  return finalWord;
}

function trimInput(phrase) {
  let letters = phrase.split("");
  //letters.pop();
  //letters.shift();
  for (let i=0; i<letters.length; i++) {
    if (letters[i] === "\"") {
      console.log('a\\');
      letters.splice(i, 1);
    } 
  }
  return letters;
}
function createIndexedMap(justLetters) { 
  let indexedMap = [];
  justLetters.map((element, index) => {
    console.log("The current iteration is: " + index);
    console.log("The current element is: " + element);
    console.log("\n");
    let indexObj = new Object();
    indexObj.index = index;
    indexObj.letter = element;
    console.log('iO=' + JSON.stringify(indexObj));
    indexedMap.push({ indexObj });
  });
  console.log('iMap=' + JSON.stringify(indexedMap));
  return indexedMap; //equivalent to list[index]
}

function getSpaceIndexList(justLetters) {
  let indexList = [];
  let letters = justLetters;
  for (let i=0; i<letters.length; i++) {
    letters[i] === " " ? indexList.push(i) : '';
  }
  return indexList;
}

function breakUpIndexedMap(indexedMap, spacesList) {
  //Only the assignment is immutable.  Value itself remain mutable.
  const IMAP = indexedMap;
  const S_LIST = spacesList;
  const slLENGTH = spacesList.length;
  let firstWordPlusSpace;
  let secondWordPlusSpace;
  let thirdWord;
  let thirdWordPlusSpace;
  let fourthWord;
  let fourthWordPlusSpace;
  let fifthWord;
  console.log("sL-" + JSON.stringify(spacesList));
  console.log("sLENGTH-" + JSON.stringify(slLENGTH));
  //let wordListObject = [];

  if (slLENGTH === 2) {
    firstWordPlusSpace = IMAP.splice(0, S_LIST[0] + 1);
    secondWordPlusSpace = IMAP.splice(0, (S_LIST[1] - S_LIST[0]));
    thirdWord = IMAP;

    app.ports.receiveStuff.send({ first: firstWordPlusSpace });
    app.ports.receiveStuff.send({ second: secondWordPlusSpace });
    app.ports.receiveStuff.send({ third: thirdWord });
    }

  else if (slLENGTH === 3) {
    console.log('sList0=' + S_LIST[0]);
    firstWordPlusSpace = IMAP.splice(0, S_LIST[0] + 1);
    secondWordPlusSpace = IMAP.splice(0, (S_LIST[1] - S_LIST[0]));
    thirdWordPlusSpace = IMAP.splice(0, (S_LIST[2] - S_LIST[1]));
    fourthWord = IMAP;

    app.ports.receiveStuff.send({ first: firstWordPlusSpace });
    app.ports.receiveStuff.send({ second: secondWordPlusSpace });
    app.ports.receiveStuff.send({ third: thirdWordPlusSpace });
    app.ports.receiveStuff.send({ fourth: fourthWord });
  }

  else if (slLENGTH === 4) {
    firstWordPlusSpace = IMAP.splice(0, S_LIST[0] + 1);
    secondWordPlusSpace = IMAP.splice(0, (S_LIST[1] - S_LIST[0]));
    thirdWordPlusSpace = IMAP.splice(0, (S_LIST[2] - S_LIST[1]));
    fourthWordPlusSpace = IMAP.splice(0, (S_LIST[3] - S_LIST[2]));
    fifthWord = IMAP;

    app.ports.receiveStuff.send({ first: firstWordPlusSpace });
    app.ports.receiveStuff.send({ second: secondWordPlusSpace });
    app.ports.receiveStuff.send({ third: thirdWordPlusSpace });
    app.ports.receiveStuff.send({ fourth: fourthWordPlusSpace });
    app.ports.receiveStuff.send({ fifth: fifthWord });
  }

  else {
    console.log('slLENGTH out of bounds');
  }

/*
  console.log('words = ' + JSON.stringify(wordListObject));

  let firstWObj = { first: firstWordPlusSpace }
  let secondWObj = { second: secondWordPlusSpace }
  let thirdWObj = S_LIST.length === 2 ? { third: thirdWord } : { third: thirdWordPlusSpace };
  let fourthWObj = S_LIST.length === 3 ? { fourth: fourthWord} : { fourth: fourthWordPlusSpace };
  let fifthWObj = { fifth: fifthWord }

  wordListObject.push( firstWObj );
  wordListObject.push( secondWObj );
  S_LIST.length === 2 ? wordListObject.push( thirdWObj ) : '';
  S_LIST.length === 3 ? wordListObject.push( thirdWObj, fourthWObj ) : '';
  fifthWord ? wordListObject.push( fifthWObj ) : '';

  console.log('wLO =' + JSON.stringify(wordListObject));
  return wordListObject;
*/
  //wordListObject = { first: firstWordPlusSpace, second: secondWordPlusSpace, third: thirdWordPlusSpace, fourth: fourthWordPlusSpace, fifth: fifthWord }
}

// If you want your app to work offline and load faster, you can change
// unregister() to register() below. Note this comes with some pitfalls.
// Learn more about service workers: https://bit.ly/CRA-PWA
serviceWorker.unregister();
