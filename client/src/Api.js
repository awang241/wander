import axios from 'axios'


const SERVER_URL = process.env.VUE_APP_SERVER_ADD;
console.log(SERVER_URL + "@@@");

  
const localAxiosInstance = axios.create({
  baseURL: SERVER_URL,  
  timeout: 1000  
});  
  
export default {
  createProfile: (user) => localAxiosInstance.post('/profiles', user),
  login: (user) => localAxiosInstance.post('login', user),
  editProfile: (userId, newData, sessionId) => localAxiosInstance.put('/profiles/' + userId, newData, {headers: {"authorization":sessionId}}),
  logout: (userId, sessionId) => localAxiosInstance.post('logout/', userId, {headers: {"authorization":sessionId}}),
  getProfile: (userId, sessionId) => localAxiosInstance.get('profiles/'+userId, {headers: {"authorization":sessionId}}),
  editEmail: (emails, userId, sessionId) => localAxiosInstance.put('profiles/'+userId+'/emails', emails, {headers: {"authorization":sessionId}}),
  editPassword: (passwordDetails, userId, sessionId) => localAxiosInstance.put('profiles/'+ userId+'/password', passwordDetails, {headers: {"authorization":sessionId}}),
  getActivitiesList: () => localAxiosInstance.get('/activities')
}