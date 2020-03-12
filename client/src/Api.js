import axios from 'axios'


const SERVER_URL = process.env.VUE_APP_SERVER_ADD;
console.log(SERVER_URL + "@@@");

  
const localAxiosInstance = axios.create({
  baseURL: SERVER_URL,  
  timeout: 1000  
});  
  
export default {


  // (C)reate  
  // createNew: (name) => localAxiosInstance.post('/students', {name}),
  createProfile: (user) => localAxiosInstance.post('createprofile', user),
  // (R)ead  
  login: (user) => localAxiosInstance.post('login', user),
  // (U)pdate  
  updateForId: (id, name) => localAxiosInstance.put('students/'+id, {name}),
  // (D)elete  
  removeForId: (id) => localAxiosInstance.delete('students/'+id),
  // (G)et
  getProfile: (id) => localAxiosInstance.get('getprofile/'+id),

  logout: (userId, sessionId) => localAxiosInstance.post('logout/', userId, {headers: {"authorization":sessionId}})
}