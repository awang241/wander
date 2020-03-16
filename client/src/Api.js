import axios from 'axios'


const SERVER_URL = process.env.VUE_APP_SERVER_ADD;
console.log(SERVER_URL + "@@@");

  
const localAxiosInstance = axios.create({
  baseURL: SERVER_URL,  
  timeout: 1000  
});  
  
export default {
  createProfile: (user) => localAxiosInstance.post('createprofile', user),
  login: (user) => localAxiosInstance.post('login', user),
  getProfile: (id, sessionId) => localAxiosInstance.get('getprofile/'+id, {headers: {"authorization":sessionId}}),
  logout: (userId, sessionId) => localAxiosInstance.post('logout/', userId, {headers: {"authorization":sessionId}})
}