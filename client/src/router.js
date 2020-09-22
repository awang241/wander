import VueRouter from 'vue-router'
import LoginComponent from "./components/General/Login.vue"
import RegistrationComponent from "./components/General/Registration.vue"
import NavBarComponent from "./components/General/NavBar.vue";
import ProfileComponent from "./components/Profile/Profile.vue";
import MainpageComponent from "./components/Main/Mainpage.vue";
import EditProfileComponent from "./components/Profile/EditProfile/EditProfile.vue";
import ActivitiesComponent from "./components/Activities/Activities";
import AdminDashboardComponent from "./components/Main/AdminDashboard";
import AddActivityComponent from "./components/Activities/AddActivity";
import ViewActivityComponent from "./components/Activities/ViewActivity.vue"
import ShareActivityComponent from "./components/Activities/ShareActivity.vue";
import ParticipationComponent from "./components/Participation/ParticipationForm.vue";
import SearchComponent from "./components/Search/Search";
import store from "./store";
import HomeFeed from "./components/Main/HomeFeed";

const routes = [
    {path:"/search", name: "search", component: SearchComponent , beforeEnter: (to, from, next) => {
            if (store.getters.getAuthenticationStatus) {
                next()
            } else {
                next({
                    name: "mainpage"
                })
            }
        }},

    {path: "/", name: "mainpage", component: MainpageComponent},
    {path: "/Login", name: "login", component: LoginComponent},
    {path: "/Registration", name: "registration", component: RegistrationComponent},
    {path: "/NavBar", name: "navbar", component: NavBarComponent},

    {path: "/Activities/:id/Participation", name: "participationForm", component: ParticipationComponent},
    {path: "/AddActivity", name: "addActivity", component: AddActivityComponent},
    {path: "/Activities", name: "activities", component:ActivitiesComponent},
    {path: '/ShareActivity/:id/:activityPrivacy', name:"shareActivity", component: ShareActivityComponent, props: true},

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
            if (!store.getters.getAuthenticationStatus) {
                next({
                    name: "mainpage"
                });
            } else if (store.getters.getAuthenticationLevel > 1){
                next({path: `/Profile/${store.getters.getUserId}`});
            } else {
                next();
            }
        }
    },
    {
        path: "/Home", name: "homeFeed", component: HomeFeed, beforeEnter: (to, from, next) => {
            if (store.getters.getAuthenticationStatus) {
                next()
            } else {
                next({
                    name: "mainpage"
                })
            }
        }
    },
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