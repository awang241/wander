import VueRouter from 'vue-router'
import LoginComponent from "./components/Login.vue"
import RegistrationComponent from "./components/Registration.vue"
import NavBarComponent from "./components/NavBar.vue";
import ProfileComponent from "./components/Profile.vue";
import MainpageComponent from "./components/Mainpage.vue";
import EditProfileComponent from "./components/editprofile/EditProfile.vue";
import ActivitiesComponent from "./components/Activities";
import AdminDashboardComponent from "./components/AdminDashboard";
import AddActivityComponent from "./components/AddActivity";
import store from "./store";


const routes = [
    {path: "/", name: "mainpage", component: MainpageComponent},
    {path: "/Login", name: "login", component: LoginComponent},
    {path: "/Registration", name: "registration", component: RegistrationComponent},
    {path: "/NavBar", name: "navbar", component: NavBarComponent},

    {path: "/AddActivity", name: "addActivity", component: AddActivityComponent},
    {path: "/Activities", name: "activities", component:ActivitiesComponent},
    {path: "/EditActivity/:", name:"editActivity", component:AddActivityComponent, props: true},
    {path: "/Profile/:id", name: "profile", component:ProfileComponent},
    // {path: "/EditProfile/:id", name: "editProfile", component: EditProfileComponent},
    // {
    //     //This route is only accessible if the user is authenticated, else it sends them back to the main page
    //     path: "/Profile/:id", name: "profile", component: ProfileComponent, beforeEnter: (to, from, next) => {
    //         console.log("Route prints below")
    //         console.log(store.getters.getAuthenticationStatus)
    //         if (store.getters.getAuthenticationStatus) {
    //             next()
    //         } else {
    //             next({
    //                 name: "mainpage"
    //             })
    //         }
    //     }
    // },
    {
        //This route is only accessible if the user is authenticated, else it sends them back to the main page
        path: "/EditProfile/:id", name: "editProfile", component: EditProfileComponent, beforeEnter: (to, from, next) => {
            if (store.getters.getAuthenticationStatus) {
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
            if (store.getters.getAuthenticationStatus) {
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