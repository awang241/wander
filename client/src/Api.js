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
  addActivityRole: (userId, activityId, token, role) => localAxiosInstance.post('/profiles/'+userId+'/activities/'+activityId+'/role', {role: role},{headers: {"authorization":token}}),

  // All delete API requests
  deleteActivity: (userId, token, activityId) => localAxiosInstance.delete('/profiles/'+userId+'/activities/'+activityId, {headers: {"authorization":token}}),
  deleteLocation: (userId, token) => localAxiosInstance.delete('/profiles/'+userId+'/location', {headers: {"authorization":token}}),
  deleteProfile: (userId, token) => localAxiosInstance.delete('/profiles/' + userId, {headers: {"authorization":token}}),
  deleteActivityMembership: (userId, activityId, token) => localAxiosInstance.delete('/profiles/' + userId + "/activities/" + activityId + "/membership", {headers: {"authorization":token}}),

  // All edit API requests
  editEmail: (emails, userId, token) => localAxiosInstance.put('profiles/'+userId+'/emails', emails, {headers: {"authorization":token}}),
  editPassword: (passwordDetails, userId, token) => localAxiosInstance.put('profiles/'+ userId+'/password', passwordDetails, {headers: {"authorization":token}}),
  editProfile: (userId, newData, token) => localAxiosInstance.put('/profiles/' + userId, newData, {headers: {"authorization":token}}),
  editProfileLocation: (userId, location, token) => localAxiosInstance.put('/profiles/' + userId + '/location', location, {headers: {"authorization":token}}),
  editProfilePermissions: (userId, role, token) => localAxiosInstance.put('/profiles/' + userId + '/role', {role: role}, {headers: {"authorization":token}}),
  editActivityMemberRole: (userId, activityId, newRole, token) => localAxiosInstance.put(`/profiles/${userId}/activities/${activityId}/role`, {role: newRole}, {headers: {"authorization": token}}),
  editActivityPrivacy: (userId, activityId, body, token) => localAxiosInstance.put('profiles/'+ userId + '/activities/' + activityId + '/privacy', {privacy: body.privacy, members: body.members}, {headers: {"authorization": token}}),
  editActivityRestrictedUsers: (userId, activityId, token, users) => localAxiosInstance.put('profiles/' + userId + '/activities/' + activityId + '/visibility', users, {headers: {"authorization":token}}),


// All get API requests
  getActivity: (activityId, token) => localAxiosInstance.get('activities/' + activityId,{headers: {"authorization":token}}),
  getActivitiesList: () => localAxiosInstance.get('activities'),
  getAllActivityMembers: (activityId, token) => localAxiosInstance.get(`/activities/${activityId}/members`, {headers: {"authorization": token}}),
  getActivityMembers: (activityId, role, token, parameters={}) => localAxiosInstance.get(`activities/${activityId}/members/${role}`, {headers: {"authorization": token}, params: parameters}),
  getActivityTypesList: () => localAxiosInstance.get('/activityTypes'),
  getAuthLevel: (token) => localAxiosInstance.get('authLevel/', {headers: {"authorization":token}}),
  getProfile: (userId, token) => localAxiosInstance.get('profiles/'+userId, {headers: {"authorization":token}}),
  getRoleCountsForActivity: (activityId, token) => localAxiosInstance.get('activities/'+activityId +'/rolecount', {headers: {"authorization":token}}),
  getUserActivitiesList: (userId, token) => localAxiosInstance.get('/profiles/' + userId + '/activities', {headers: {"authorization":token}}),
  getUserProfiles: (token, parameters = {}) => localAxiosInstance.get('profiles/', {headers: {"authorization":token}, params: parameters}),
  getNextActivities: (userId, token, parameters = {}) => localAxiosInstance.get('/profiles/' + userId + '/activities', {headers: {"authorization":token}, params: parameters}),
  getSingleUserActivityRole: (userId, activityId, token) => localAxiosInstance.get(`/profiles/${userId}/activities/${activityId}/role`, {headers: {"authorization":token}}),


  // All login/logout API requests
  login: (user) => localAxiosInstance.post('login', user),
  logout: (userId, token) => localAxiosInstance.post('logout/', userId, {headers: {"authorization":token}}),

  // All update API requests
  updateActivity: (userId, token, newData, activityId) => localAxiosInstance.put('/profiles/'+userId+'/activities/'+ activityId, newData, {headers: {"authorization":token}}),

  // All verification APi requests
  verifyToken: (token) => localAxiosInstance.get('/token', {headers: {"authorization": token}}),
  verifyEmail: (email) => localAxiosInstance.get('/email/' + email)
}