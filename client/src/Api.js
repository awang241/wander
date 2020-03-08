import axios from 'axios'  
  
const SERVER_URL = "localhost:9499/";
console.log(SERVER_URL + "@@@");
  
const localAxiosInstance = axios.create({
  baseURL: SERVER_URL,  
  timeout: 1000  
});  
  
export default {


  // (C)reate  
  // createNew: (name) => localAxiosInstance.post('/students', {name}),
  createProfile: (user) => localAxiosInstance.post('/createprofile', user),
  // (R)ead  
  getAll: () => localAxiosInstance.get('students', {
    transformResponse: [function (data) {  
      return data? JSON.parse(data)._embedded.students : data;  
    }]  
  }),  
  // (U)pdate  
  updateForId: (id, name) => localAxiosInstance.put('students/'+id, {name}),
  // (D)elete  
  removeForId: (id) => localAxiosInstance.delete('students/'+id)
}