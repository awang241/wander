import VueRouter from 'vue-router'
import LoginComponent from "./components/Login.vue"
import RegistrationComponent from "./components/Registration.vue"
import NavBarComponent from "./components/NavBar.vue";
import ProfileComponent from "./components/Profile.vue";
import MainpageComponent from "./components/Mainpage.vue";
import EditProfileComponent from "./components/editprofile/EditProfile.vue";
import AdminDashboardComponent from "./components/AdminDashboard";
import authenticationStore from "./store/authenticationStore";


const routes = [
    {path: "/Mainpage", name: "mainpage", component: MainpageComponent},
    {path: "/Login", name: "login", component: LoginComponent},
    {path: "/Registration", name: "registration", component: RegistrationComponent},
    {path: "/NavBar", name: "navbar", component: NavBarComponent},
    {path: "/EditProfile/:id", name: "editProfile", component: EditProfileComponent},
    {
        //This route is only accessible if the user is authenticated, else it sends them back to the main page
        path: "/Profile", name: "profile", component: ProfileComponent, beforeEnter: (to, from, next) => {
            if (authenticationStore.methods.isAuthenticated()) {
                next()
            } else {
                next({
                    name: "mainpage"
                })
            }
        }
    },
    {
        //This route is only accessible if the user is authenticated, else it sends them back to the main page
        path: "/AdminDashboard", name: "adminDashboard", component: AdminDashboardComponent, beforeEnter: (to, from, next) => {
            if (authenticationStore.methods.isAuthenticated()) {
                next()
            } else {
                next({
                    name: "mainpage"
                })
            }
        }
    }
]

const router = new VueRouter({
    routes,
    mode: 'history'
})

export default router;