import * as admin from "firebase-admin"

export const firebase = admin.initializeApp({
    credential: admin.credential.cert('firebase-admin.json'),
    databaseURL: 'https://planets-io.firebaseio.com'
});

export const database = firebase.firestore()