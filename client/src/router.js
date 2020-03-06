import VueRouter from 'vue-router'
import LoginComponent from "./components/Login.vue"
import RegistrationComponent from "./components/Registration.vue"
import NavBarComponent from "./components/NavBar.vue";
import ProfileComponent from "./components/Profile.vue";
import MainpageComponent from "./components/Mainpage.vue";


const routes = [
        { path: "/Mainpage", name: "mainpage", component: MainpageComponent },
        { path: "/Login", name: "login", component: LoginComponent },
        { path: "/Registration", name: "registration", component: RegistrationComponent },
        { path: "/NavBar", name: "navbar", component: NavBarComponent },
        { path: "/Profile", name: "profile", component: ProfileComponent },
]

const router = new VueRouter( {
    routes,
    mode: 'history'
})

export default router;
