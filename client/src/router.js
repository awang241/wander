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
import ProfileSearchComponent from "./components/ProfileSearch"
import ViewActivityComponent from "./components/ViewActivity.vue"
import ShareActivityComponent from "./components/ShareActivity.vue";
import ParticipationComponent from "./components/ParticipationForm.vue";
import store from "./store";
import HomeFeed from "./components/HomeFeed";


const routes = [
    {path: "/", name: "mainpage", component: MainpageComponent},
    {path: "/Login", name: "login", component: LoginComponent},
    {path: "/Registration", name: "registration", component: RegistrationComponent},
    {path: "/NavBar", name: "navbar", component: NavBarComponent},

    {path: "/Activities/:id/Participation", name: "participationForm", component: ParticipationComponent},
    {path: "/AddActivity", name: "addActivity", component: AddActivityComponent},
    {path: "/Activities", name: "activities", component:ActivitiesComponent},
    {path: '/ShareActivity/:id/:activityPrivacy', name:"shareActivity", component: ShareActivityComponent, props: true},
    {path: "/ProfileSearch", name: "profileSearch", component:ProfileSearchComponent, beforeEnter: (to, from, next) => {
            if (store.getters.getAuthenticationStatus) {
                next()
            } else {
                next({
                    name: "login"
                })
            }
    }},
    {path: "/EditActivity/:", name:"editActivity", component:AddActivityComponent, props: true},
    {path: "/Activities/:id", name:"viewActivity", component:ViewActivityComponent, props: true},
    {path: "/Profile/:id", name: "profile", component:ProfileComponent, props: true},
    {
        //This route is only accessible if the user is authenticated, else it sends them back to the main page
        path: "/EditProfile/:id", name: "editProfile", props: true, component: EditProfileComponent, beforeEnter: (to, from, next) => {
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
    },
    {path: "/home", name: "homeFeed", component: HomeFeed}
];

const router = new VueRouter({
    base: process.env.VUE_APP_BASE_URL,
    mode: 'history',
    scrollBehaviour(to, from, savedPosition) {
        if (savedPosition) {
            return savedPosition;
        } else {
            return {x: 0, y: 0};
        }
    },
    routes
});

export default router;