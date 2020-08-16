import axios from 'axios'


const SERVER_URL = process.env.VUE_APP_SERVER_ADD;
console.log(SERVER_URL + "@@@");


const localAxiosInstance = axios.create({
  baseURL: SERVER_URL,
  timeout: 10000
});

export default {
  // All create API requests
  createActivity: (userId, data, token) => localAxiosInstance.post('/profiles/'+userId+'/activities', data, {headers: {"authorization":token}}),
  createProfile: (user) => localAxiosInstance.post('/profiles', user),
  createActivityParticipation: (userId, activityId, participation, token) => localAxiosInstance.post('/profiles/'+ userId + '/activities/' + activityId + '/participation', participation, {headers: {'authorization': token}}),

  // All delete API requests
  deleteActivity: (userId, token, activityId) => localAxiosInstance.delete('/profiles/'+userId+'/activities/'+activityId, {headers: {"authorization":token}}),
  deleteLocation: (userId, token) => localAxiosInstance.delete('/profiles/'+userId+'/location', {headers: {"authorization":token}}),
  deleteProfile: (userId, token) => localAxiosInstance.delete('/profiles/' + userId, {headers: {"authorization":token}}),
  deleteActivityParticipation: (userId, activityId, token, participationId) => localAxiosInstance.delete('/profiles/'+ userId + '/activities/' + activityId + '/participation/' + participationId, {headers: {'authorization': token}}),

  // All edit API requests
  editEmail: (emails, userId, token) => localAxiosInstance.put('profiles/'+userId+'/emails', emails, {headers: {"authorization":token}}),
  editPassword: (passwordDetails, userId, token) => localAxiosInstance.put('profiles/'+ userId+'/password', passwordDetails, {headers: {"authorization":token}}),
  editProfile: (userId, newData, token) => localAxiosInstance.put('/profiles/' + userId, newData, {headers: {"authorization":token}}),
  editProfileLocation: (userId, location, token) => localAxiosInstance.put('/profiles/' + userId + '/location', location, {headers: {"authorization":token}}),
  editProfilePermissions: (userId, role, token) => localAxiosInstance.put('/profiles/' + userId + '/role', {role: role}, {headers: {"authorization":token}}),
  editActivityPrivacy: (userId, activityId, body, token) => localAxiosInstance.put('profiles/'+ userId + '/activities/' + activityId + '/privacy', {privacy: body.privacy, members: body.members}, {headers: {"authorization": token}}),
  editActivityRestrictedUsers: (userId, activityId, token, users) => localAxiosInstance.put('profiles/' + userId + '/activities/' + activityId + '/visibility', users, {headers: {"authorization":token}}),

// All get API requests
  getActivity: (activityId, token) => localAxiosInstance.get('activities/' + activityId,{headers: {"authorization":token}}),
  getActivitiesList: () => localAxiosInstance.get('activities'),
  getActivityTypesList: () => localAxiosInstance.get('/activityTypes'),
  getAuthLevel: (token) => localAxiosInstance.get('authLevel/', {headers: {"authorization":token}}),
  getProfile: (userId, token) => localAxiosInstance.get('profiles/'+userId, {headers: {"authorization":token}}),
  getRoleCountsForActivity: (activityId, token) => localAxiosInstance.get('activities/'+activityId +'/rolecount', {headers: {"authorization":token}}),
  getUserActivitiesList: (userId, token) => localAxiosInstance.get('/profiles/' + userId + '/activities', {headers: {"authorization":token}}),
  getUserProfiles: (token, parameters = {}) => localAxiosInstance.get('profiles/', {headers: {"authorization":token}, params: parameters}),
  getNextActivities: (userId, token, parameters = {}) => localAxiosInstance.get('/profiles/' + userId + '/activities', {headers: {"authorization":token}, params: parameters}),
  getActivityParticipation: (userId, activityId, token, participationId) => localAxiosInstance.get('/profiles/'+ userId + '/activities/' + activityId + '/participation/' + participationId, {headers: {'authorization': token}}),
  getAllActivityParticipations: (activityId, token) => localAxiosInstance.get('activities/' + activityId + '/participations', {headers: {'authorization': token}}),
  getMyActivityRole: (activityId, token) => localAxiosInstance.get('activities/' + activityId + '/myRole',{headers: {'authorization': token}}), //TODO hook this up to the endpoint in the other branch

  // All login/logout API requests
  login: (user) => localAxiosInstance.post('login', user),
  logout: (userId, token) => localAxiosInstance.post('logout/', userId, {headers: {"authorization":token}}),

  // All update API requests
  updateActivity: (userId, token, newData, activityId) => localAxiosInstance.put('/profiles/'+userId+'/activities/'+ activityId, newData, {headers: {"authorization":token}}),
  updateActivityParticipation: (userId, activityId, token, participationId, participation) => localAxiosInstance.put('/profiles/'+ userId + '/activities/' + activityId + '/participation/' + participationId, participation, {headers: {'authorization': token}}),


  // All verification APi requests
  verifyToken: (token) => localAxiosInstance.get('/token', {headers: {"authorization": token}}),
  verifyEmail: (email) => localAxiosInstance.get('/email/' + email)
}