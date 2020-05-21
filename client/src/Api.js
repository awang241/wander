import axios from 'axios'


const SERVER_URL = process.env.VUE_APP_SERVER_ADD;
console.log(SERVER_URL + "@@@");

  
const localAxiosInstance = axios.create({
  baseURL: SERVER_URL,  
  timeout: 10000
});  
  
export default {
  createProfile: (user) => localAxiosInstance.post('/profiles', user),
  login: (user) => localAxiosInstance.post('login', user),
  editProfile: (userId, newData, token) => localAxiosInstance.put('/profiles/' + userId, newData, {headers: {"authorization":token}}),
  editProfileLocation: (userId, location, token) => localAxiosInstance.put('/profiles/' + userId + '/location', location, {headers: {"authorization":token}}),
  logout: (userId, token) => localAxiosInstance.post('logout/', userId, {headers: {"authorization":token}}),
  getProfile: (userId, token) => localAxiosInstance.get('profiles/'+userId, {headers: {"authorization":token}}),
  editEmail: (emails, userId, token) => localAxiosInstance.put('profiles/'+userId+'/emails', emails, {headers: {"authorization":token}}),
  editPassword: (passwordDetails, userId, token) => localAxiosInstance.put('profiles/'+ userId+'/password', passwordDetails, {headers: {"authorization":token}}),
  getActivityTypesList: () => localAxiosInstance.get('/activityTypes'),
  getUserProfiles: (token) => localAxiosInstance.get('profiles/', {headers: {"authorization":token}}),
  getAuthLevel: (token) => localAxiosInstance.get('authLevel/', {headers: {"authorization":token}}),
  createActivity: (userId, data, token) => localAxiosInstance.post('/profiles/'+userId+'/activities', data, {headers: {"authorization":token}}),
  getActivitiesList: () => localAxiosInstance.get('activities'),
  getUserActivitiesList: (userId, token) => localAxiosInstance.get('/profiles/' + userId + '/activities', {headers: {"authorization":token}}),
  updateActivity: (userId, token, newData, activityId) => localAxiosInstance.put('/profiles/'+userId+'/activities/'+ activityId, newData, {headers: {"authorization":token}}),
  deleteActivity: (userId, token, activityId) => localAxiosInstance.delete('/profiles/'+userId+'/activities/'+activityId, {headers: {"authorization":token}}),
  verifyToken: (token) => localAxiosInstance.get('/token', {headers: {"authorization": token}}),
  deleteLocation: (userId, token) => localAxiosInstance.delete('/profiles'+userId+'/location', {headers: {"authorization":token}})
}